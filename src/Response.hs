{-# LANGUAGE OverloadedStrings #-}

module Response (Response (..), ResponseType (..), send, parseResponseType) where

import ArgOptions
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe
import Data.Time
import Duration
import GHC.Exts
import Table
import Task
import Text.Printf (printf)
import Worktime

data ResponseType
  = Json
  | Text
  deriving (Show, Read)

data Response
  = TasksResponse UTCTime Project [Task]
  | TaskResponse UTCTime Task
  | WtimeResponse UTCTime MoreOpt [DailyWorktime]
  | StatusResponse UTCTime (Maybe Task)
  | VersionResponse String
  | ContextResponse Project
  | MessageResponse String
  | ErrorResponse String

send :: ResponseType -> Response -> IO ()
send rtype (TasksResponse now ctx tasks) = showTasks now rtype ctx tasks
send rtype (TaskResponse now task) = showTask now rtype task
send rtype (WtimeResponse now moreOpt wtimes) = showWtime now rtype moreOpt wtimes
send rtype (StatusResponse now task) = showStatus now rtype task
send rtype (VersionResponse version) = showVersion rtype version
send rtype (ContextResponse proj) = showContext rtype proj
send rtype (MessageResponse msg) = showMessage rtype msg
send rtype (ErrorResponse msg) = showError rtype msg

-- Tasks

showTasks :: UTCTime -> ResponseType -> Project -> [Task] -> IO ()
showTasks now Text Nothing tasks = do
  putStrLn ""
  putStrLn $ showTasksWithProjText now tasks
showTasks now Text (Just ctx) tasks = do
  putStrLn $ "Tasks from \x1b[34m" ++ ctx ++ "\x1b[0m project:"
  putStrLn ""
  putStrLn $ showTasksText now tasks
showTasks now Json _ tasks = BL.putStr $ encode $ showTasksJson now tasks

showTasksText :: UTCTime -> [Task] -> String
showTasksText now tasks = render $ head : body
  where
    head = map (bold . underline . cell) ["ID", "DESC", "ACTIVE", "DUE", "WORKTIME"]
    body = map rows tasks
    rows task =
      [ red $ cell $ getId task,
        cell $ getDesc task,
        green $ cell $ showApproxTimeDiff now $ getActive task,
        (if isDuePassed now task then bgRed . white else yellow) $ cell $ showApproxTimeDiffRel now $ getDue task,
        yellow $ cell $ showApproxDuration $ getTaskWtime now task
      ]

showTasksWithProjText :: UTCTime -> [Task] -> String
showTasksWithProjText now tasks = render $ head : body
  where
    head = map (bold . underline . cell) ["ID", "DESC", "PROJECT", "ACTIVE", "DUE", "WORKTIME"]
    body = map rows tasks
    rows task =
      [ red $ cell $ getId task,
        cell $ getDesc task,
        blue $ cell $ fromMaybe "" $ getProject task,
        green $ cell $ showApproxTimeDiff now $ getActive task,
        (if isDuePassed now task then bgRed . white else yellow) $ cell $ showApproxTimeDiffRel now $ getDue task,
        yellow $ cell $ showApproxDuration $ getTaskWtime now task
      ]

showTasksJson :: UTCTime -> [Task] -> Data.Aeson.Value
showTasksJson now tasks =
  object
    [ "success" .= (1 :: Int),
      "tasks" .= (Array $ fromList $ map (showTaskJson now) tasks)
    ]

-- Task

showTask :: UTCTime -> ResponseType -> Task -> IO ()
showTask now Text task = do
  putStrLn ""
  putStrLn $ showTaskText now task
showTask now Json task = BL.putStr $ encode $ showTaskJson now task

showTaskText :: UTCTime -> Task -> String
showTaskText now task = render $ head : body
  where
    head = map (bold . underline . cell) ["KEY", "VALUE"]
    body =
      transpose
        [ map cell ["ID", "DESC", "PROJECT", "ACTIVE", "DUE", "WORKTIME", "DONE", "DELETED"],
          [ red $ cell $ getId task,
            cell $ getDesc task,
            blue $ cell $ fromMaybe "" $ getProject task,
            green $ cell $ fromMaybe "" $ show <$> getActive task,
            (if isDuePassed now task then bgRed . white else yellow) $ cell $ fromMaybe "" $ show <$> getDue task,
            yellow $ cell $ showFullDuration $ getTaskWtime now task,
            cell $ fromMaybe "" $ show <$> getDone task,
            cell $ fromMaybe "" $ show <$> getDeleted task
          ]
        ]

showTaskJson :: UTCTime -> Task -> Data.Aeson.Value
showTaskJson now task =
  object
    [ "success" .= (1 :: Int),
      "task"
        .= object
          [ "id" .= getId task,
            "desc" .= getDesc task,
            "project" .= getProject task,
            "active" .= showActiveJson now (getActive task),
            "wtime" .= showTaskWtimeJson (getTaskWtime now task),
            "done" .= if isNothing (getDone task) then 1 else 0 :: Int
          ]
    ]

-- Worktime

showWtime :: UTCTime -> ResponseType -> MoreOpt -> [DailyWorktime] -> IO ()
showWtime now Text False dwtimes = putStrLn $ showDailyWtimeText now dwtimes
showWtime now Text True dwtimes = putStrLn $ showFullDailyWtimeText now dwtimes
showWtime now Json _ dwtimes = BL.putStr $ encode $ Array $ fromList $ map (showDailyWtimeJson now) dwtimes

showDailyWtimeText :: UTCTime -> [DailyWorktime] -> String
showDailyWtimeText now dwtimes = render $ head : body ++ foot
  where
    head = map (underline . bold . cell) ["DATE", "WORKTIME"]
    body = map rows dwtimes
    rows dwtime =
      [ cell $ fst dwtime,
        yellow $ cell $ showFullDuration $ sum $ map getWtimeDuration $ snd dwtime
      ]
    foot =
      [ replicate 2 $ ext 8 . cell $ "—",
        [ bold . cell $ "TOTAL RAW",
          bold . cell $ showFullDuration total
        ],
        [ bold . cell $ "TOTAL WDAY",
          bold . cell $ showFullDuration (total * 3.2)
        ]
      ]
    total = sum $ map getWtimeDuration $ concatMap snd dwtimes

showFullDailyWtimeText :: UTCTime -> [DailyWorktime] -> String
showFullDailyWtimeText now dwtimes = render $ head : body ++ foot
  where
    head = map (underline . bold . cell) ["DATE", "ID", "DESC", "WORKTIME"]
    body = concatMap rows dwtimes

    rows dwtime = (map rows' $ snd dwtime) ++ subtotal
      where
        subtotal =
          [ [ bold $ cell "SUBTOTAL",
              cell "",
              cell "",
              bold $ cell $ showFullDuration $ sum $ map getWtimeDuration $ snd dwtime
            ],
            replicate 4 $ ext 8 . cell $ "—"
          ]
        rows' wtime =
          [ cell $ fst dwtime,
            red $ cell $ getWtimeId wtime,
            cell $ getWtimeDesc wtime,
            yellow $ cell $ showFullDuration $ getWtimeDuration wtime
          ]
    foot =
      [ [ bold $ cell $ "TOTAL RAW",
          cell "",
          cell "",
          bold $ cell $ showFullDuration total
        ],
        [ bold $ cell $ "TOTAL WDAY",
          cell "",
          cell "",
          bold $ cell $ showFullDuration (total * 3.2)
        ]
      ]
    total = sum $ map getWtimeDuration $ concatMap snd dwtimes

showDailyWtimeJson :: UTCTime -> DailyWorktime -> Data.Aeson.Value
showDailyWtimeJson now (day, wtimes) =
  object
    [ "success" .= (1 :: Int),
      "worktime"
        .= object
          [ "day" .= day,
            "total" .= showWtimesJson wtimes
          ]
    ]

-- Status

showStatus :: UTCTime -> ResponseType -> Maybe Task -> IO ()
showStatus now Text task = putStrLn $ showStatusText now task
showStatus now Json task = BL.putStr $ encode $ showStatusJson now task

showStatusText :: UTCTime -> Maybe Task -> String
showStatusText now Nothing = ""
showStatusText now (Just task) = getDesc task ++ ": " ++ showApproxTimeDiff now (getActive task)

showStatusJson :: UTCTime -> Maybe Task -> Maybe Data.Aeson.Value
showStatusJson now = fmap showStatusJson'
  where
    showStatusJson' task =
      object
        [ "success" .= (1 :: Int),
          "desc" .= getDesc task,
          "active" .= showActiveJson now (getActive task)
        ]

-- Version

showVersion :: ResponseType -> String -> IO ()
showVersion Text version = putStrLn version
showVersion Json version = BL.putStr $ encode $ object ["success" .= (1 :: Int), "version" .= version]

-- Context

showContext :: ResponseType -> Project -> IO ()
showContext rtype proj = case rtype of
  Json -> BL.putStr $ encode $ showMessageJson plainMsg
  Text -> putStrLn styledMsg
  where
    plainMsg = case proj of
      Nothing -> "Context cleared"
      Just proj -> printf "Project \x1b[32mupdated\x1b[0m set to " ++ proj
    styledMsg = case proj of
      Nothing -> "Context \x1b[31mcleared\x1b[0m"
      Just proj -> "Context \x1b[32mupdated\x1b[0m [\x1b[34m" ++ proj ++ "\x1b[0m]"

-- Command message

showMessage :: ResponseType -> String -> IO ()
showMessage Text msg = putStrLn msg
showMessage Json msg = BL.putStr $ encode $ showMessageJson msg

showMessageJson :: String -> Data.Aeson.Value
showMessageJson msg =
  object
    [ "success" .= (1 :: Int),
      "message" .= msg
    ]

-- Error

showError :: ResponseType -> String -> IO ()
showError rtype msg = case rtype of
  Json -> BL.putStr $ encode $ showErrorJson msg
  Text -> putStrLn $ showErrorText msg

showErrorJson :: String -> Data.Aeson.Value
showErrorJson msg =
  object
    [ "success" .= (0 :: Int),
      "message" .= ("Error: " ++ msg)
    ]

showErrorText :: String -> String
showErrorText msg = "\x1b[31mError: " ++ msg ++ "\x1b[0m"

-- Helpers

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
    micro = showMicroTime now active
    approx = showApproxTimeDiff now active
    full = showFullTimeRel now active

showTaskWtimeJson :: Duration -> Data.Aeson.Value
showTaskWtimeJson micro = showDurationJson micro approx full
  where
    approx = showApproxDuration micro
    full = showFullDuration micro

showWtimesJson :: [Worktime] -> Data.Aeson.Value
showWtimesJson wtimes = showDurationJson micro approx full
  where
    micro = sum $ map getWtimeDuration wtimes
    approx = showApproxDuration micro
    full = showFullDuration micro

parseResponseType :: JsonOpt -> ResponseType
parseResponseType True = Json
parseResponseType False = Text
