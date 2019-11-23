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
type Ref = Int
type Pos = Int
type Desc = String
type Tag = String
type Due = Maybe UTCTime
type Active = Bool
type Done = Bool
type Wtime = Micro

data Task =
  Task { _id :: Id
       , _ref :: Ref
       , _pos :: Pos
       , _desc :: Desc
       , _tags :: [Tag]
       , _due :: Maybe Due
       , _active :: Bool
       , _done :: Bool
       , _wtime :: Wtime
       , _starts :: [UTCTime]
       , _stops :: [UTCTime]
       } deriving (Show, Read, Eq)

newtype WtimeRecord = WtimeRecord {toWtimeRecord :: Wtime}
instance ToJSON WtimeRecord where
  toJSON (WtimeRecord wtime) = object
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
    , "wtime" .= WtimeRecord wtime
    ]

instance ToJSON DailyWtimeRecord where
  toJSON (DailyWtimeRecord (date, wtime)) =
    object ["date" .= date, "wtime" .= WtimeRecord wtime]

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
  filteredTasks = if null tags then tasks else filter byTags tasks
  byTags        = not . null . intersect tags . _tags


mapWithWtime :: UTCTime -> [Task] -> [Task]
mapWithWtime now = map withWtime
  where withWtime task = task { _wtime = getTotalWtime now task }

getTotalWtime :: UTCTime -> Task -> Wtime
getTotalWtime now task = realToFrac $ sum $ zipWith diffUTCTime stops starts
 where
  starts = _starts task
  stops  = _stops task ++ [ now | _active task ]

getWtimePerDay :: UTCTime -> [Task] -> [DailyWtime]
getWtimePerDay now tasks = foldl fWtimePerDay [] $ zip starts stops
 where
  (starts, stops) = foldl fByStartsAndStops ([], []) tasks
  fByStartsAndStops (starts, stops) t =
    (starts ++ _starts t, stops ++ _stops t ++ [ now | _active t ])

type DailyWtime = (String, Micro)
newtype DailyWtimeRecord = DailyWtimeRecord {toDailyWtimeRecord :: DailyWtime}
newtype DailyWtimeTotalRecord = DailyWtimeTotalRecord {toDailyWtimeTotalRecord :: Wtime}
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
    , unwords $ map tail $ _tags task
    , if _active task then "✔" else ""
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
