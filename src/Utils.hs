module Utils where

import           Data.List
import           Data.Maybe
import           System.Environment

getFilePath :: String -> IO String
getFilePath file = (++ "/" ++ file) <$> getConfigDirPath

getConfigDirPath :: IO String
getConfigDirPath = lookupEnv "XDG_CONFIG_HOME" >>= withDefault
 where
  withDefault maybePath = case maybePath of
    Just path -> (++ "/unfog") <$> return path
    Nothing   -> (++ "/.config/unfog") . fromMaybe "/tmp" <$> lookupEnv "HOME"

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

generateId :: [Int] -> Int
generateId ids = generateId' currIds genIds
 where
  currIds = sort ids
  genIds  = [1 ..]

generateId' :: [Int] -> [Int] -> Int
generateId' [] []          = 1
generateId' [] (genId : _) = genId
generateId' (currId : currIds) (genId : genIds)
  | currId == genId = generateId' currIds genIds
  | otherwise       = genId

startsByPlus :: String -> Bool
startsByPlus "+"       = False
startsByPlus ('+' : _) = True
startsByPlus _         = False

elog :: String -> String -> IO ()
elog ""      message = putStrLn $ "unfog: " ++ message
elog command message = putStrLn $ "unfog: " ++ command ++ ": " ++ message
