module Store
  ( readEvents
  , writeEvents
  )
where

import           Control.Exception
import           System.Directory
import           System.Environment             ( lookupEnv )
import           System.IO.Error

import           Event
import           State
import           Utils

readEvents :: IO [Event]
readEvents = mapLineToEvent <$> getStoreFileContent
  where mapLineToEvent = map Prelude.read . lines

writeEvents :: [Event] -> IO ()
writeEvents = foldr write' (return ())
 where
  write' event _ = getStoreFilePath >>= appendToStore event
  appendToStore event = flip appendFile $ show event ++ "\n"

getStoreFileContent :: IO String
getStoreFileContent = do
  storePath    <- getStoreFilePath
  tmpStorePath <- (++ "/unfog.store") <$> getTemporaryDirectory
  copyFile' storePath tmpStorePath
  storeContent <- readFile' tmpStorePath
  removeFile' tmpStorePath
  return storeContent

getStoreFilePath :: IO String
getStoreFilePath = do
  configPath <- lookupEnv "XDG_CONFIG_HOME" >>= bindXDGConfigDirPath
  createDirectoryIfMissing True configPath
  return $ configPath ++ "/store"
 where
  bindXDGConfigDirPath maybePath = case maybePath of
    Just path -> return $ path ++ "/unfog"
    Nothing   -> lookupEnv "HOME" >>= bindHomeDirPath
  bindHomeDirPath maybePath = case maybePath of
    Just home -> return $ home ++ "/.config/unfog"
    Nothing   -> return "/tmp"

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
