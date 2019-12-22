{-# LANGUAGE OverloadedStrings #-}

module Task where

import qualified Data.ByteString.Lazy.Char8    as BL
import           Control.Exception
import           Data.Aeson
import           Data.List
import           Data.Time
import           Data.Duration
import           Text.PrettyPrint.Boxes
import           Data.Fixed

import           Utils

type Id = Int
type Ref = Int
type Pos = Int
type Desc = String
type Tag = String
type Active = Bool
type Done = Bool
type Duration = Micro

data Task =
  Task { _id :: Id
       , _ref :: Ref
       , _pos :: Pos
       , _desc :: Desc
       , _tags :: [Tag]
       , _due :: Maybe Duration
       , _active :: Duration
       , _done :: Done
       , _wtime :: Duration
       , _starts :: [UTCTime]
       , _stops :: [UTCTime]
       , _createdAt :: UTCTime
       } deriving (Show, Read, Eq)

newtype TimeRecord = TimeRecord {toTimeRecord :: Duration}
instance ToJSON TimeRecord where
  toJSON (TimeRecord time) = object
    [ "approx" .= (approximativeDuration time ++ " ago")
    , "human" .= (humanReadableDuration time ++ " ago")
    , "micro" .= time
    ]

newtype DurationRecord = DurationRecord {toWtimeRecord :: Duration}
instance ToJSON DurationRecord where
  toJSON (DurationRecord wtime) = object
    [ "approx" .= approximativeDuration wtime
    , "human" .= humanReadableDuration wtime
    , "micro" .= wtime
    ]

instance ToJSON Task where
  toJSON (Task id ref pos desc tags due active done wtime _ _ createdAt) =
    object
      [ "id" .= id
      , "ref" .= ref
      , "pos" .= pos
      , "desc" .= desc
      , "tags" .= tags
      , "active" .= TimeRecord active
      , "done" .= if done then 1 else 0 :: Int
      , "wtime" .= DurationRecord wtime
      , "createdAt" .= createdAt
      ]

instance ToJSON DailyWtimeRecord where
  toJSON (DailyWtimeRecord (date, wtime)) =
    object ["date" .= date, "wtime" .= DurationRecord wtime]

emptyTask :: Task
emptyTask = Task { _id        = 0
                 , _ref       = 0
                 , _pos       = -1
                 , _desc      = ""
                 , _tags      = []
                 , _due       = Nothing
                 , _active    = 0
                 , _done      = False
                 , _wtime     = 0
                 , _starts    = []
                 , _stops     = []
                 , _createdAt = read "0000-00-00 00:00:00" :: UTCTime
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

findByRef :: Ref -> [Task] -> Maybe Task
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
  tags'         = tags \\ ["done"]
  filteredTasks = if null tags' then tasks else filter byTags tasks
  byTags        = not . null . intersect tags' . _tags

filterByMinDate :: Maybe UTCTime -> [UTCTime] -> [UTCTime]
filterByMinDate maybeDate dates = case maybeDate of
  Nothing   -> dates
  Just date -> filter (date <) dates

mapWithWtime :: UTCTime -> [Task] -> [Task]
mapWithWtime now = map withWtime
  where withWtime task = task { _wtime = getTotalWtime now task }

getTotalWtime :: UTCTime -> Task -> Duration
getTotalWtime now task = realToFrac $ sum $ zipWith diffUTCTime stops starts
 where
  starts = _starts task
  stops  = _stops task ++ [ now | _active task > 0 ]

getWtimePerDay :: UTCTime -> Maybe UTCTime -> [Task] -> [DailyWtime]
getWtimePerDay now min tasks = foldl fWtimePerDay [] $ zip starts stops
 where
  (starts, stops) = foldl byStartsAndStops ([], []) tasks
  byStartsAndStops (starts, stops) t =
    ( starts ++ filterByMinDate min (_starts t)
    , stops ++ filterByMinDate min (_stops t ++ [ now | _active t > 0 ])
    )

type DailyWtime = (String, Duration)
newtype DailyWtimeRecord = DailyWtimeRecord {toDailyWtimeRecord :: DailyWtime}
newtype DailyWtimeTotalRecord = DailyWtimeTotalRecord {toDailyWtimeTotalRecord :: Duration}
fWtimePerDay :: [DailyWtime] -> (UTCTime, UTCTime) -> [DailyWtime]
fWtimePerDay acc (start, stop) = case lookup key acc of
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
      , fWtimePerDay acc (nextDay, stop)
      )

prettyPrintTasks :: [Task] -> IO ()
prettyPrintTasks tasks =
  putStrLn $ render $ table $ header : map prettyPrint tasks
 where
  header = ["ID", "DESC", "TAGS", "ACTIVE"]
  prettyPrint task =
    [ show $ _id task
    , _desc task
    , unwords $ _tags task
    , if _active task > 0
      then approximativeDuration (_active task) ++ " ago"
      else ""
    ]

prettyPrintWtime :: [DailyWtime] -> IO ()
prettyPrintWtime wtime =
  putStrLn
    $  render
    $  table
    $  [header]
    ++ map prettyPrint wtime
    ++ [["TOTAL", humanReadableDuration total]]
 where
  header = ["DATE", "WORKTIME"]
  prettyPrint (date, wtime) = [date, humanReadableDuration $ realToFrac wtime]
  total = foldl (\acc (_, x) -> acc + x) 0 wtime
