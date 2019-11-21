{-# LANGUAGE OverloadedStrings #-}

module Task where

import qualified Data.ByteString.Lazy.Char8    as BL
import           Control.Exception
import           Data.Aeson
import           Data.Duration
import           Data.List
import           Data.Time
import           Text.PrettyPrint.Boxes
import           Data.Fixed

import           Utils

type Id = Int
type Reference = Int
type Position = Int
type Description = String
type Tag = String
type Due = Maybe UTCTime
type Active = Bool
type Done = Bool
type Worktime = Micro
newtype WorktimeRecord = WorktimeRecord {toWorktimeRecord :: Worktime}

data Task =
  Task { _id :: Id
       , _ref :: Reference
       , _pos :: Position
       , _desc :: Description
       , _tags :: [Tag]
       , _due :: Maybe Due
       , _active :: Bool
       , _done :: Bool
       , _wtime :: Worktime
       , _starts :: [UTCTime]
       , _stops :: [UTCTime]
       } deriving (Show, Read, Eq)

instance ToJSON WorktimeRecord where
  toJSON (WorktimeRecord wtime) = object
    [ "approx" .= approximativeDuration wtime
    , "human" .= humanReadableDuration wtime
    , "micro" .= wtime
    ]

instance ToJSON Task where
  toJSON (Task id ref pos desc tags due active done wtime _ _) = object
    [ "id" .= id
    , "ref" .= ref
    , "pos" .= pos
    , "desc" .= desc
    , "tags" .= map tail tags
    , "active" .= if active then 1 else 0 :: Int
    , "done" .= if done then 1 else 0 :: Int
    , "wtime" .= WorktimeRecord wtime
    ]

instance ToJSON DailyWtimeRecord where
  toJSON (DailyWtimeRecord (date, wtime)) =
    object ["date" .= date, "wtime" .= WorktimeRecord wtime]

emptyTask :: Task
emptyTask = Task { _id     = 0
                 , _ref    = 0
                 , _pos    = -1
                 , _desc   = ""
                 , _tags   = []
                 , _due    = Nothing
                 , _active = False
                 , _done   = False
                 , _wtime  = 0
                 , _starts = []
                 , _stops  = []
                 }

generateId :: [Task] -> Id
generateId tasks = generateId' (sort $ map _id tasks) [1 ..]
 where
  generateId' [] []           = 1
  generateId' [] (nextId : _) = nextId
  generateId' (currId : currIds) (nextId : nextIds)
    | currId == nextId = generateId' currIds nextIds
    | otherwise        = nextId

findById :: Id -> [Task] -> Maybe Task
findById id = find $ (==) id . _id

findByRef :: Reference -> [Task] -> Maybe Task
findByRef ref = find $ (==) ref . _ref

filterByDone :: Done -> [Task] -> [Task]
filterByDone showDone tasks = filteredTasks
 where
  filteredTasks = filter byDone tasks
  byDone        = if showDone then _done else not . _done

filterByIds :: [Id] -> [Task] -> [Task]
filterByIds ids = filter (flip elem ids . _id)

filterByTags :: [Tag] -> [Task] -> [Task]
filterByTags tags tasks = filteredTasks
 where
  filteredTasks = if null tags then tasks else filter byTags tasks
  byTags        = not . null . intersect tags . _tags


mapWithWorktime :: UTCTime -> [Task] -> [Task]
mapWithWorktime now = map withWorktime
  where withWorktime task = task { _wtime = getTotalWorktime now task }

getTotalWorktime :: UTCTime -> Task -> Worktime
getTotalWorktime now task = realToFrac $ sum $ zipWith diffUTCTime stops starts
 where
  starts = _starts task
  stops  = _stops task ++ [ now | _active task ]

getWorktimePerDay :: UTCTime -> [Task] -> [DailyWtime]
getWorktimePerDay now tasks = foldl fWorktimePerDay [] $ zip starts stops
 where
  (starts, stops) = foldl fByStartsAndStops ([], []) tasks
  fByStartsAndStops (starts, stops) t =
    (starts ++ _starts t, stops ++ _stops t ++ [ now | _active t ])

type DailyWtime = (String, Micro)
newtype DailyWtimeRecord = DailyWtimeRecord {toDailyWtimeRecord :: DailyWtime}
newtype DailyWtimeTotalRecord = DailyWtimeTotalRecord {toDailyWtimeTotalRecord :: Worktime}
fWorktimePerDay :: [DailyWtime] -> (UTCTime, UTCTime) -> [DailyWtime]
fWorktimePerDay acc (start, stop) = case lookup key acc of
  Nothing       -> (key, nextSecs) : nextAcc
  Just prevSecs -> (key, prevSecs + nextSecs) : filter ((/=) key . fst) nextAcc
 where
  key                 = show currDay
  currDay             = utctDay start
  endOfDay            = read $ show currDay ++ " 23:59:59.999999999" :: UTCTime
  nextDay = read $ show (addDays 1 currDay) ++ " 00:00:00" :: UTCTime
  (nextSecs, nextAcc) = if stop < endOfDay
    then (realToFrac $ diffUTCTime stop start, acc)
    else
      ( realToFrac $ diffUTCTime endOfDay start
      , fWorktimePerDay acc (nextDay, stop)
      )

prettyPrintTasks :: [Task] -> IO ()
prettyPrintTasks tasks =
  putStrLn $ render $ table $ header : map prettyPrint tasks
 where
  header = ["ID", "DESC", "TAGS", "ACTIVE"]
  prettyPrint task =
    [ show $ _id task
    , _desc task
    , unwords $ map tail $ _tags task
    , if _active task then "✔" else ""
    ]

prettyPrintWtime :: [DailyWtime] -> IO ()
prettyPrintWtime wtime =
  putStrLn $ render $ table $ header : map prettyPrint wtime
 where
  header = ["DATE", "WORKTIME"]
  prettyPrint (date, wtime) = [date, humanReadableDuration $ realToFrac wtime]
