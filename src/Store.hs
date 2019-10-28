module Store
  ( readAllEvents
  , writeEvents
  )
where

import           Control.Exception
import           System.IO.Error
import           System.Directory

import           Event
import           State
import           Utils

readAllEvents :: IO [Event]
readAllEvents = mapLineToEvent <$> getStoreFileContent
  where mapLineToEvent = map Prelude.read . lines

writeEvents :: [Event] -> IO ()
writeEvents = foldl writeEvent' (return ()) where writeEvent' _ = writeEvent

writeEvent :: Event -> IO ()
writeEvent event = getFilePath "store" >>= appendEventToStore
 where
  eventStr           = show event ++ "\n"
  appendEventToStore = flip appendFile eventStr

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
