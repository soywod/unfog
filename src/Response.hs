{-# LANGUAGE OverloadedStrings #-}

module Response where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Duration
import Task

-- data Response
--   = ResponseMsg String
--   | ResponseTask Task
--   | ResponseTasks [Task]
--   | ResponseTasksId [Id]
--   | ResponseTasksTags [Tag]
--   | ResponseWtime [DailyWtime]
--   | ResponseStatus Task
--   | ResponseErr String

-- data ResponseType = JSON | Text

-- printMsg :: ResponseType -> String -> IO ()
-- printMsg rtype msg = case rtype of
--   JSON -> BL.putStr $ encode $ ResponseMsg msg
--   Text -> putStrLn $ "unfog: " ++ msg

-- printTasksId :: ResponseType -> [Id] -> IO ()
-- printTasksId rtype ids = case rtype of
--   JSON -> BL.putStr . encode . ResponseTasksId $ ids
--   Text -> putStr . unwords . map show $ ids

-- printTasksTags :: ResponseType -> [Tag] -> IO ()
-- printTasksTags rtype tags = case rtype of
--   JSON -> BL.putStr . encode . ResponseTasksTags . map ((:) '+') $ tags
--   Text -> putStr . unwords . map ((:) '+') $ tags

-- printTasks :: ResponseType -> String -> [Task] -> IO ()
-- printTasks rtype msg tasks = case rtype of
--   JSON -> BL.putStr $ encode $ ResponseTasks tasks
--   Text -> do
--     putStrLn msg
--     putStrLn ""
--     prettyPrintTasks tasks
--     putStrLn ""

-- printTask :: ResponseType -> Task -> IO ()
-- printTask rtype task = case rtype of
--   JSON -> BL.putStr $ encode $ ResponseTask task
--   Text -> prettyPrintTask task

-- printWtime :: ResponseType -> Bool -> String -> [DailyWtime] -> IO ()
-- printWtime rtype more msg wtime = case rtype of
--   JSON -> BL.putStr $ encode $ ResponseWtime wtime
--   Text -> do
--     let prettyPrint = if more then prettyPrintFullWtime else prettyPrintWtime
--     putStrLn msg
--     putStrLn ""
--     prettyPrint wtime
--     putStrLn ""

-- printEmptyStatus :: ResponseType -> IO ()
-- printEmptyStatus rtype = case rtype of
--   JSON -> BL.putStr $ encode $ ResponseErr "no active task found"
--   Text -> putStrLn ""

-- printStatus :: ResponseType -> Task -> IO ()
-- printStatus rtype task = case rtype of
--   JSON -> BL.putStr $ encode $ ResponseStatus task
--   Text -> putStrLn $ desc task ++ ": " ++ showApproxDuration (active task)

-- printVersion :: ResponseType -> String -> IO ()
-- printVersion rtype version = case rtype of
--   JSON -> BL.putStr $ encode $ ResponseMsg version
--   Text -> putStrLn version

-- printErr :: ResponseType -> String -> IO ()
-- printErr rtype err = case rtype of
--   JSON -> BL.putStr $ encode $ ResponseErr err
--   Text -> putStrLn $ "\x1b[31munfog: " ++ err ++ "\x1b[0m"

-- instance ToJSON Response where
--   toJSON (ResponseMsg msg) = object ["ok" .= (1 :: Int), "data" .= msg]
--   toJSON (ResponseTask task) = object ["ok" .= (1 :: Int), "data" .= task]
--   toJSON (ResponseTasksId ids) = object ["ok" .= (1 :: Int), "data" .= ids]
--   toJSON (ResponseTasksTags tags) = object ["ok" .= (1 :: Int), "data" .= tags]
--   toJSON (ResponseTasks tasks) = object ["ok" .= (1 :: Int), "data" .= tasks]
--   toJSON (ResponseWtime wtime) =
--     object
--       [ "ok" .= (1 :: Int),
--         "data"
--           .= object
--             [ "wtimes" .= map DailyWtimeRecord wtime,
--               "total" .= DurationRecord (sum $ map getWtime $ concatMap snd wtime)
--             ]
--       ]
--   toJSON (ResponseStatus task) =
--     object
--       [ "ok" .= (1 :: Int),
--         "data"
--           .= object
--             ["active" .= DurationRecord (active task), "desc" .= desc task]
--       ]
--   toJSON (ResponseErr err) = object ["ok" .= (0 :: Int), "data" .= err]

-- getResponseType :: [String] -> ResponseType
-- getResponseType args
--   | "--json" `elem` args = JSON
--   | otherwise = Text
