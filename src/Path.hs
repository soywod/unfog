module Path where

import           System.Directory               ( createDirectoryIfMissing )
import           System.Environment             ( lookupEnv )

getFilePath :: String -> IO String
getFilePath file = do
  configPath <- getConfigDirPath
  createDirectoryIfMissing True configPath
  return $ configPath ++ "/" ++ file

getConfigDirPath :: IO String
getConfigDirPath = lookupEnv "XDG_CONFIG_HOME" >>= bindConfigDirPath
 where
  bindConfigDirPath maybePath = case maybePath of
    Just path -> return $ path ++ "/unfog"
    Nothing   -> lookupEnv "HOME" >>= bindHomeDirPath
  bindHomeDirPath maybePath = case maybePath of
    Just home -> return $ home ++ "/.config/unfog"
    Nothing   -> return "/tmp"
