module Store
  ( readAll
  , write
  )
where

import           Control.Exception
import           System.IO.Error
import           System.Directory

import           Event
import           State
import           Utils

readAll :: IO [Event]
readAll = mapLineToEvent <$> getStoreFileContent
  where mapLineToEvent = map Prelude.read . lines

write :: [Event] -> IO ()
write = foldr write' (return ())
 where
  write' event _ = getFilePath "store" >>= appendToStore event
  appendToStore event = flip appendFile $ show event ++ "\n"

getStoreFileContent :: IO String
getStoreFileContent = do
  let tmpStorePath = "/tmp/unfog-store"
  storePath <- getFilePath "store"
  copyFile' storePath tmpStorePath
  storeContent <- readFile' tmpStorePath
  removeFile' tmpStorePath
  return storeContent

copyFile' :: String -> String -> IO ()
copyFile' src dest = copyFile src dest `catch` handleErrors
 where
  handleErrors err | isDoesNotExistError err = return ()
                   | otherwise               = throwIO err

readFile' :: String -> IO String
readFile' path = readFile path `catch` handleErrors
 where
  handleErrors err | isDoesNotExistError err = return ""
                   | otherwise               = throwIO err

removeFile' :: String -> IO ()
removeFile' path = removeFile path `catch` handleErrors
 where
  handleErrors err | isDoesNotExistError err = return ()
                   | otherwise               = throwIO err
