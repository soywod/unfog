{-# LANGUAGE OverloadedStrings #-}

module Response where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe
import Data.Time
import Duration
import GHC.Exts
import Table
import Task

data ResponseType
  = Json
  | Text
  deriving (Show, Read)

data Response
  = TasksResponse UTCTime [Task]
  | TaskResponse UTCTime Task

instance ToJSON Response where
  toJSON (TasksResponse now tasks) = Array $ fromList $ map (taskToJson now) tasks
    where
      taskToJson now task =
        object
          [ "id" .= getId task,
            "desc" .= getDesc task,
            "tags" .= getTags task,
            "active" .= ActiveResponse now (getActive task),
            "done" .= if getDone task then 1 else 0 :: Int
          ]
  toJSON (TaskResponse now task) =
    object
      [ "id" .= getId task,
        "desc" .= getDesc task,
        "tags" .= getTags task,
        "active" .= ActiveResponse now (getActive task),
        "done" .= if getDone task then 1 else 0 :: Int
      ]

data ActiveResponse = ActiveResponse UTCTime Active

instance ToJSON ActiveResponse where
  toJSON (ActiveResponse now active) =
    object
      [ "micro" .= showMicroActive now active,
        "approx" .= showApproxActiveRel now active,
        "full" .= showFullActiveRel now active
      ]

response :: ResponseType -> Response -> String
response rtype (TasksResponse now tasks) = showTasksTable now rtype tasks
response rtype (TaskResponse now task) = showTaskTable now rtype task

showMicroActive :: UTCTime -> Active -> Duration
showMicroActive _ Nothing = 0
showMicroActive now (Just active) = realToFrac $ diffUTCTime active now

showApproxActiveRel :: UTCTime -> Active -> String
showApproxActiveRel _ Nothing = ""
showApproxActiveRel now (Just active) = showApproxDurationRel $ showMicroActive now (Just active)

showFullActiveRel :: UTCTime -> Active -> String
showFullActiveRel _ Nothing = ""
showFullActiveRel now (Just active) = showFullDurationRel $ showMicroActive now (Just active)

showTasksTable :: UTCTime -> ResponseType -> [Task] -> String
showTasksTable now Json tasks = BL.unpack $ encode $ TasksResponse now tasks
showTasksTable now Text tasks = render $ tableTasks tasks
  where
    tableTasks tasks = tableTaskHead : map tableTaskRow tasks
    tableTaskHead = map (bold . underline . cell) ["ID", "DESC", "TAGS", "ACTIVE"]
    tableTaskRow task =
      [ red $ cell $ getId task,
        cell $ getDesc task,
        blue $ cell $ unwords $ getTags task,
        green $ cell $ showApproxActiveRel now $ getActive task
      ]

showTaskTable :: UTCTime -> ResponseType -> Task -> String
showTaskTable now Json task = BL.unpack $ encode $ TaskResponse now task
showTaskTable now Text task = render $ tableTaskHead : tableTaskRow
  where
    tableTask now task = tableTaskHead : tableTaskRow
    tableTaskHead = map (bold . underline . cell) ["KEY", "VALUE"]
    keys = map cell ["ID", "DESC", "TAGS", "ACTIVE"]
    values =
      [ red $ cell $ getId task,
        cell $ getDesc task,
        blue $ cell $ unwords $ getTags task,
        green $ cell $ showFullActiveRel now $ getActive task
      ]
    tableTaskRow = transpose [keys, values]

-- printMsg :: ResponseType -> String -> IO ()
-- printMsg rtype msg = case rtype of
--   Json -> BL.putStr $ encode $ ResponseMsg msg
--   Text -> putStrLn $ "unfog: " ++ msg

-- printTasksId :: ResponseType -> [Id] -> IO ()
-- printTasksId rtype ids = case rtype of
--   Json -> BL.putStr . encode . ResponseTasksId $ ids
--   Text -> putStr . unwords . map show $ ids

-- printTasksTags :: ResponseType -> [Tag] -> IO ()
-- printTasksTags rtype tags = case rtype of
--   Json -> BL.putStr . encode . ResponseTasksTags . map ((:) '+') $ tags
--   Text -> putStr . unwords . map ((:) '+') $ tags

-- printTasks :: ResponseType -> String -> [Task] -> IO ()
-- printTasks rtype msg tasks = case rtype of
--   Json -> BL.putStr $ encode $ ResponseTasks tasks
--   Text -> do
--     putStrLn msg
--     putStrLn ""
--     prettyPrintTasks tasks
--     putStrLn ""

-- printTask :: ResponseType -> Task -> IO ()
-- printTask rtype task = case rtype of
--   Json -> BL.putStr $ encode $ ResponseTask task
--   Text -> prettyPrintTask task

-- printWtime :: ResponseType -> Bool -> String -> [DailyWtime] -> IO ()
-- printWtime rtype more msg wtime = case rtype of
--   Json -> BL.putStr $ encode $ ResponseWtime wtime
--   Text -> do
--     let prettyPrint = if more then prettyPrintFullWtime else prettyPrintWtime
--     putStrLn msg
--     putStrLn ""
--     prettyPrint wtime
--     putStrLn ""

-- printEmptyStatus :: ResponseType -> IO ()
-- printEmptyStatus rtype = case rtype of
--   Json -> BL.putStr $ encode $ ResponseErr "no active task found"
--   Text -> putStrLn ""

-- printStatus :: ResponseType -> Task -> IO ()
-- printStatus rtype task = case rtype of
--   Json -> BL.putStr $ encode $ ResponseStatus task
--   Text -> putStrLn $ desc task ++ ": " ++ showApproxDuration (active task)

-- printVersion :: ResponseType -> String -> IO ()
-- printVersion rtype version = case rtype of
--   Json -> BL.putStr $ encode $ ResponseMsg version
--   Text -> putStrLn version

-- printErr :: ResponseType -> String -> IO ()
-- printErr rtype err = case rtype of
--   Json -> BL.putStr $ encode $ ResponseErr err
--   Text -> putStrLn $ "\x1b[31munfog: " ++ err ++ "\x1b[0m"

-- instance ToJson Response where
--   toJson (ResponseMsg msg) = object ["ok" .= (1 :: Int), "data" .= msg]
--   toJson (ResponseTask task) = object ["ok" .= (1 :: Int), "data" .= task]
--   toJson (ResponseTasksId ids) = object ["ok" .= (1 :: Int), "data" .= ids]
--   toJson (ResponseTasksTags tags) = object ["ok" .= (1 :: Int), "data" .= tags]
--   toJson (ResponseTasks tasks) = object ["ok" .= (1 :: Int), "data" .= tasks]
--   toJson (ResponseWtime wtime) =
--     object
--       [ "ok" .= (1 :: Int),
--         "data"
--           .= object
--             [ "wtimes" .= map DailyWtimeRecord wtime,
--               "total" .= DurationRecord (sum $ map getWtime $ concatMap snd wtime)
--             ]
--       ]
--   toJson (ResponseStatus task) =
--     object
--       [ "ok" .= (1 :: Int),
--         "data"
--           .= object
--             ["active" .= DurationRecord (active task), "desc" .= desc task]
--       ]
--   toJson (ResponseErr err) = object ["ok" .= (0 :: Int), "data" .= err]

-- getResponseType :: [String] -> ResponseType
-- getResponseType args
--   | "--json" `elem` args = Json
--   | otherwise = Text

