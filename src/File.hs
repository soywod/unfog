module File (getFileContent, getFilePath) where

import Control.Exception
import System.Directory
import System.Environment (lookupEnv)
import System.IO.Error

getFileContent :: String -> IO String
getFileContent fname = do
  fpath <- getFilePath fname
  fpath' <- (++ "/unfog." ++ fname) <$> getTemporaryDirectory
  copyFile' fpath fpath'
  fcontent <- readFile' fpath'
  removeFile' fpath'
  return fcontent

getFilePath :: String -> IO String
getFilePath fname = do
  fpath <- lookupEnv "XDG_CONFIG_HOME" >>= bindXDGConfigDirPath
  createDirectoryIfMissing True fpath
  return $ fpath ++ "/" ++ fname
  where
    bindXDGConfigDirPath maybePath = case maybePath of
      Just path -> return $ path ++ "/unfog"
      Nothing -> lookupEnv "HOME" >>= bindHomeDirPath
    bindHomeDirPath maybePath = case maybePath of
      Just home -> return $ home ++ "/.config/unfog"
      Nothing -> return "/tmp"

copyFile' :: String -> String -> IO ()
copyFile' src dest = copyFile src dest `catch` handleErrors
  where
    handleErrors err
      | isDoesNotExistError err = return ()
      | otherwise = throwIO err

readFile' :: String -> IO String
readFile' path = readFile path `catch` handleErrors
  where
    handleErrors err
      | isDoesNotExistError err = return ""
      | otherwise = throwIO err

removeFile' :: String -> IO ()
removeFile' path = removeFile path `catch` handleErrors
  where
    handleErrors err
      | isDoesNotExistError err = return ()
      | otherwise = throwIO err
