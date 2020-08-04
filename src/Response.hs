{-# LANGUAGE OverloadedStrings #-}

module Response (Response (..), ResponseType (..), send) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe
import Data.Time
import Duration
import GHC.Exts
import Table
import Task
import Worktime

data ResponseType
  = Json
  | Text
  deriving (Show, Read)

data Response
  = TasksResponse UTCTime [Task]
  | TaskResponse UTCTime Task
  | WtimeResponse UTCTime [DailyWorktime]
  | StatusResponse UTCTime (Maybe Task)
  | VersionResponse String
  | ErrorResponse String String

send :: ResponseType -> Response -> IO ()
send rtype (TasksResponse now tasks) = showTasks now rtype tasks
send rtype (TaskResponse now task) = showTask now rtype task
send rtype (WtimeResponse now wtimes) = showWtime now rtype wtimes
send rtype (StatusResponse now task) = showStatus now rtype task
send rtype (VersionResponse version) = showVersion rtype version
send rtype (ErrorResponse cmd err) = showError rtype cmd err

-- Tasks

showTasks :: UTCTime -> ResponseType -> [Task] -> IO ()
showTasks now Text tasks = putStrLn $ showTasksText now tasks
showTasks now Json tasks = BL.putStr $ encode $ Array $ fromList $ map (showTaskJson now) tasks

showTasksText now tasks = render $ head : body
  where
    head = map (bold . underline . cell) ["ID", "DESC", "TAGS", "ACTIVE"]
    body = map rows tasks
    rows task =
      [ red $ cell $ getId task,
        cell $ getDesc task,
        blue $ cell $ unwords $ getTags task,
        green $ cell $ showApproxActiveRel now $ getActive task
      ]

showTask :: UTCTime -> ResponseType -> Task -> IO ()
showTask now Text task = putStrLn $ showTaskText now task
showTask now Json task = BL.putStr $ encode $ showTaskJson now task

showTaskText now task = render $ head : body
  where
    head = map (bold . underline . cell) ["KEY", "VALUE"]
    body =
      transpose
        [ map cell ["ID", "DESC", "TAGS", "ACTIVE"],
          [ red $ cell $ getId task,
            cell $ getDesc task,
            blue $ cell $ unwords $ getTags task,
            green $ cell $ showFullActiveRel now $ getActive task
          ]
        ]

showTaskJson :: UTCTime -> Task -> Data.Aeson.Value
showTaskJson now task =
  object
    [ "id" .= getId task,
      "desc" .= getDesc task,
      "tags" .= getTags task,
      "active" .= showActiveJson now (getActive task),
      "done" .= if isNothing (getDone task) then 1 else 0 :: Int
    ]

-- Worktime

showWtime :: UTCTime -> ResponseType -> [DailyWorktime] -> IO ()
showWtime now Text dwtimes = putStrLn $ showDailyWtimeText now dwtimes
showWtime now Json dwtimes = BL.putStr $ encode $ Array $ fromList $ map (showDailyWtimeJson now) dwtimes

showDailyWtimeText :: UTCTime -> [DailyWorktime] -> String
showDailyWtimeText now dwtimes = render $ head : body ++ foot
  where
    head = map (underline . bold . cell) ["DATE", "WORKTIME"]
    body = map rows dwtimes
    foot =
      [ replicate 2 $ ext 8 . cell $ replicate 3 '-',
        [bold . cell $ "TOTAL RAW", bold . cell $ showFullDuration total],
        [bold . cell $ "TOTAL WDAY", bold . cell $ showFullDuration (total * 3.2)]
      ]
    rows dwtime = [cell $ fst dwtime, yellow $ cell $ showFullDuration $ sum $ map getWtimeDuration $ snd dwtime]
    total = sum $ map getWtimeDuration $ concatMap snd dwtimes

showDailyWtimeJson :: UTCTime -> DailyWorktime -> Data.Aeson.Value
showDailyWtimeJson now (day, wtimes) =
  object
    [ "day" .= day,
      "total" .= showWtimesJson wtimes
    ]

-- Status

showStatus :: UTCTime -> ResponseType -> Maybe Task -> IO ()
showStatus now Text task = putStrLn $ showStatusText now task
showStatus now Json task = BL.putStr $ encode $ showStatusJson now task

showStatusJson :: UTCTime -> Maybe Task -> Maybe Data.Aeson.Value
showStatusJson now = fmap showStatusJson'
  where
    showStatusJson' task =
      object
        [ "desc" .= getDesc task,
          "active" .= showActiveJson now (getActive task)
        ]

showStatusText :: UTCTime -> Maybe Task -> String
showStatusText now task = case task of
  Nothing -> ""
  Just task -> getDesc task ++ ": " ++ showApproxActiveRel now (getActive task)

-- Version

showVersion :: ResponseType -> String -> IO ()
showVersion Text version = putStrLn $ showVersionText version
showVersion Json version = BL.putStr $ encode $ showVersionJson version

showVersionJson :: String -> Data.Aeson.Value
showVersionJson version = object ["version" .= version]

showVersionText :: String -> String
showVersionText version = version

-- Error

showError :: ResponseType -> String -> String -> IO ()
showError rtype cmd err = case rtype of
  Json -> BL.putStr $ encode $ showErrorJson cmd err
  Text -> putStrLn $ showErrorText cmd err

showErrorJson :: String -> String -> Data.Aeson.Value
showErrorJson cmd err =
  object
    [ "success" .= (0 :: Int),
      "command" .= cmd,
      "data" .= err
    ]

showErrorText :: String -> String -> String
showErrorText cmd err = "\x1b[31munfog: " ++ cmd ++ ": " ++ err ++ "\x1b[0m"

-- Helpers

showMicroActive :: UTCTime -> Active -> Duration
showMicroActive _ Nothing = 0
showMicroActive now (Just active) = realToFrac $ diffUTCTime active now

showApproxActiveRel :: UTCTime -> Active -> String
showApproxActiveRel _ Nothing = ""
showApproxActiveRel now (Just active) = showApproxDurationRel $ showMicroActive now (Just active)

showFullActiveRel :: UTCTime -> Active -> String
showFullActiveRel _ Nothing = ""
showFullActiveRel now (Just active) = showFullDurationRel $ showMicroActive now (Just active)

showDurationJson :: Duration -> String -> String -> Data.Aeson.Value
showDurationJson micro approx full =
  object
    [ "micro" .= micro,
      "approx" .= approx,
      "full" .= full
    ]

showActiveJson :: UTCTime -> Active -> Maybe Data.Aeson.Value
showActiveJson _ Nothing = Nothing
showActiveJson now active = Just $ showDurationJson micro approx full
  where
    micro = showMicroActive now active
    approx = showApproxActiveRel now active
    full = showFullActiveRel now active

showWtimesJson :: [Worktime] -> Data.Aeson.Value
showWtimesJson wtimes = showDurationJson micro approx full
  where
    micro = sum $ map getWtimeDuration wtimes
    approx = showApproxDuration micro
    full = showFullDuration micro

-- showWorktimeJson :: Worktime -> Data.Aeson.Value
-- showWorktimeJson date wtime =
--   object
--     ["date" .= date, "wtime" .= DurationRecord (sum $ map getWtime wtime)]

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
