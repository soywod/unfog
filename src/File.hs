module File where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Maybe (fromMaybe)
import System.Directory (copyFile, createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)

getDefaultDir :: IO String
getDefaultDir = do
  pathXDG <- lookupEnv "XDG_CONFIG_HOME"
  pathHome <- lookupEnv "HOME"
  pathTmp <- getTemporaryDirectory
  let fromXDG = (++ "/unfog") <$> pathXDG
  let fromHome = (++ "/.config/unfog") <$> pathHome
  let fpath = fromMaybe pathTmp (fromXDG <|> fromHome)
  createDirectoryIfMissing True fpath
  return fpath

getFullPath :: String -> IO String
getFullPath fname = (++ "/" ++ fname) <$> getDefaultDir

readFromPath :: String -> IO String
readFromPath fpath = do
  fpath' <- (++ "/unfog") <$> getTemporaryDirectory
  copyFile fpath fpath'
  fcontent <- readFile fpath'
  removeFile fpath'
  return fcontent

readFromName :: String -> IO String
readFromName = readFromPath <=< getFullPath
