{-# LANGUAGE OverloadedStrings #-}

module Response where

import           Data.Aeson
import           Data.List
import qualified Data.ByteString.Lazy.Char8    as BL

import           Task

data Response
  = ResponseMsg String
  | ResponseTask Task
  | ResponseTasks [Task]
  | ResponseWtime [DailyWtime]
  | ResponseErr String

data ResponseType = JSON | Text

printMsg :: ResponseType -> String -> IO ()
printMsg rtype msg = case rtype of
  JSON -> BL.putStr $ encode $ ResponseMsg msg
  Text -> putStrLn $ "unfog: " ++ msg

printTask :: ResponseType -> Task -> IO ()
printTask rtype task = case rtype of
  JSON -> BL.putStr $ encode $ ResponseTask task
  Text -> prettyPrintTasks [task]

printTasks :: ResponseType -> [Task] -> IO ()
printTasks rtype tasks = case rtype of
  JSON -> BL.putStr $ encode $ ResponseTasks tasks
  Text -> prettyPrintTasks tasks

printWtime :: ResponseType -> String -> [DailyWtime] -> IO ()
printWtime rtype msg wtime = case rtype of
  JSON -> BL.putStr $ encode $ ResponseWtime wtime
  Text -> do
    putStrLn msg
    prettyPrintWtime wtime

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
      [ "wtimes" .= map DailyWtimeRecord wtime
      , "total" .= DurationRecord (foldl (\t (_, w) -> t + w) 0 wtime)
      ]
    ]
  toJSON (ResponseErr err) = object ["ok" .= (0 :: Int), "data" .= err]

getResponseType :: [String] -> ResponseType
getResponseType args | "--json" `elem` args = JSON
                     | otherwise            = Text
