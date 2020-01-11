{-# LANGUAGE OverloadedStrings #-}

module Response where

import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Aeson
import           Data.Duration
import           Data.List

import           Task

data Response
  = ResponseMsg String
  | ResponseTask Task
  | ResponseTasks [Task]
  | ResponseWtime [DailyWtime]
  | ResponseStatus Task
  | ResponseErr String

data ResponseType = JSON | Text

printMsg :: ResponseType -> String -> IO ()
printMsg rtype msg = case rtype of
  JSON -> BL.putStr $ encode $ ResponseMsg msg
  Text -> putStrLn $ "unfog: " ++ msg

printTasks :: ResponseType -> String -> [Task] -> IO ()
printTasks rtype msg tasks = case rtype of
  JSON -> BL.putStr $ encode $ ResponseTasks tasks
  Text -> do
    putStrLn msg
    putStrLn ""
    prettyPrintTasks tasks
    putStrLn ""

printTask :: ResponseType -> Task -> IO ()
printTask rtype task = case rtype of
  JSON -> BL.putStr $ encode $ ResponseTask task
  Text -> prettyPrintTask task

printWtime :: ResponseType -> String -> [DailyWtime] -> IO ()
printWtime rtype msg wtime = case rtype of
  JSON -> BL.putStr $ encode $ ResponseWtime wtime
  Text -> do
    putStrLn msg
    putStrLn ""
    prettyPrintWtime wtime
    putStrLn ""

printEmptyStatus :: ResponseType -> IO ()
printEmptyStatus rtype = case rtype of
  JSON -> BL.putStr $ encode $ ResponseErr "no active task found"
  Text -> putStrLn ""

printStatus :: ResponseType -> Task -> IO ()
printStatus rtype task = case rtype of
  JSON -> BL.putStr $ encode $ ResponseStatus task
  Text -> putStrLn $ _desc task ++ ": " ++ approximativeDuration (_active task)

printVersion :: ResponseType -> String -> IO ()
printVersion rtype version = case rtype of
  JSON -> BL.putStr $ encode $ ResponseMsg version
  Text -> putStrLn version

printErr :: ResponseType -> String -> IO ()
printErr rtype err = case rtype of
  JSON -> BL.putStr $ encode $ ResponseErr err
  Text -> putStrLn $ "\x1b[31munfog: " ++ err ++ "\x1b[0m"

instance ToJSON Response where
  toJSON (ResponseMsg   msg  ) = object ["ok" .= (1 :: Int), "data" .= msg]
  toJSON (ResponseTask  task ) = object ["ok" .= (1 :: Int), "data" .= task]
  toJSON (ResponseTasks tasks) = object ["ok" .= (1 :: Int), "data" .= tasks]
  toJSON (ResponseWtime wtime) = object
    [ "ok" .= (1 :: Int)
    , "data" .= object
      ["wtimes" .= map DailyWtimeRecord wtime, "total" .= DurationRecord 0]
    ]
  toJSON (ResponseStatus task) = object
    [ "ok" .= (1 :: Int)
    , "data" .= object
      ["active" .= DurationRecord (_active task), "desc" .= _desc task]
    ]
  toJSON (ResponseErr err) = object ["ok" .= (0 :: Int), "data" .= err]

getResponseType :: [String] -> ResponseType
getResponseType args | "--json" `elem` args = JSON
                     | otherwise            = Text
