module File where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import System.Directory (copyFile, createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)

getPath :: String -> IO String
getPath fname = do
  xdgPath <- lookupEnv "XDG_CONFIG_HOME"
  homePath <- lookupEnv "HOME"
  tpmPath <- getTemporaryDirectory
  let configPathFromXDG = (++ "/unfog") <$> xdgPath
  let configPathFromHome = (++ "/.config/unfog") <$> homePath
  let configPath = fromMaybe tpmPath (configPathFromXDG <|> configPathFromHome)
  createDirectoryIfMissing True configPath
  return $ configPath ++ "/" ++ fname

getContent :: String -> IO String
getContent fname = do
  fpath <- getPath fname
  fpath' <- (++ "/unfog." ++ fname) <$> getTemporaryDirectory
  copyFile fpath fpath'
  fcontent <- readFile fpath'
  removeFile fpath'
  return fcontent
