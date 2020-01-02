{-# LANGUAGE OverloadedStrings #-}

module Task where

import           Data.Aeson
import           Data.Duration
import           Data.Fixed
import           Data.List
import           Data.Maybe
import           Data.Time

import           Table

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
       } deriving (Show, Read, Eq)

newtype TimeRecord = TimeRecord {toTimeRecord :: Maybe Duration}
instance ToJSON TimeRecord where
  toJSON (TimeRecord time) = object
    [ "approx" .= (printApproxTime time)
    , "human" .= (printHumanTime time)
    , "micro" .= fromMaybe 0 time
    ]

newtype DurationRecord = DurationRecord {toWtimeRecord :: Duration}
instance ToJSON DurationRecord where
  toJSON (DurationRecord wtime) = object
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
    , "tags" .= tags
    , "active" .= TimeRecord (Just (-active))
    , "due" .= (TimeRecord due)
    , "done" .= if done then 1 else 0 :: Int
    , "wtime" .= DurationRecord wtime
    ]

instance ToJSON DailyWtimeRecord where
  toJSON (DailyWtimeRecord (date, wtime)) =
    object ["date" .= date, "wtime" .= DurationRecord wtime]

emptyTask :: Task
emptyTask = Task { _id     = 0
                 , _ref    = 0
                 , _pos    = -1
                 , _desc   = ""
                 , _tags   = []
                 , _due    = Nothing
                 , _active = 0
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

filterByRefs :: [Ref] -> [Task] -> [Task]
filterByRefs refs = filter (flip elem refs . _ref)

filterByTags :: [Tag] -> [Task] -> [Task]
filterByTags tags tasks = filteredTasks
 where
  tags'         = tags \\ ["done"]
  filteredTasks = if null tags' then tasks else filter byTags tasks
  byTags        = not . null . intersect tags' . _tags

mapWithWtime :: UTCTime -> [Task] -> [Task]
mapWithWtime now = map withWtime
  where withWtime task = task { _wtime = getTotalWtime now task }

getTotalWtime :: UTCTime -> Task -> Duration
getTotalWtime now task = realToFrac $ sum $ zipWith diffUTCTime stops starts
 where
  starts = _starts task
  stops  = _stops task ++ [ now | _active task > 0 ]

getWtimePerDay
  :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> [Task] -> [DailyWtime]
getWtimePerDay now min max tasks = withoutEmpty $ wtime
 where
  wtime           = foldl fWtimePerDay [] $ zip starts stops
  withoutEmpty    = filter $ (\(_, d) -> d > 0)
  (starts, stops) = foldl byStartsAndStops ([], []) tasks
  byStartsAndStops (starts, stops) t =
    ( starts ++ withMinMax min max (_starts t)
    , stops ++ withMinMax min max (_stops t ++ [ now | _active t > 0 ])
    )

withMinMax :: Maybe UTCTime -> Maybe UTCTime -> [UTCTime] -> [UTCTime]
withMinMax maybeMin maybeMax = map withMinMax'
 where
  withMinMax' date =
    let max = minimum $ [fromMaybe date maybeMax, date]
    in  maximum $ [fromMaybe max maybeMin, max]

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

printActive :: Micro -> String
printActive active | active > 0 = approximativeDuration active ++ " ago"
                   | otherwise  = ""

printApproxTime :: Maybe Duration -> String
printApproxTime Nothing    = ""
printApproxTime (Just due) = if due > 0 then "in " ++ due' else due' ++ " ago"
  where due' = approximativeDuration $ abs due

printHumanTime :: Maybe Duration -> String
printHumanTime Nothing    = ""
printHumanTime (Just due) = if due > 0 then "in " ++ due' else due' ++ " ago"
  where due' = humanReadableDuration $ abs due

prettyPrintTasks :: [Task] -> IO ()
prettyPrintTasks = render . tableTasks

tableTasks :: [Task] -> [[Cell]]
tableTasks tasks = head : body
 where
  head = tableTaskHead
  body = map tableTaskRow tasks

tableTaskHead :: [Cell]
tableTaskHead =
  map (bold . underline . cell) ["ID", "DESC", "TAGS", "ACTIVE", "DUE"]

tableTaskRow :: Task -> [Cell]
tableTaskRow task = cells
 where
  [id, desc, tags, active, due] = taskToStrings task
  cells =
    [ red . cell $ id
    , cell desc
    , blue . cell $ tags
    , green . cell $ active
    , (if fromMaybe 0 (_due task) < 0 then bgRed . white else yellow)
      . cell
      $ due
    ]

taskToStrings :: Task -> [String]
taskToStrings task =
  [ show $ _id task
  , _desc task
  , unwords $ _tags task
  , printActive $ _active task
  , printApproxTime $ _due task
  ]

prettyPrintWtime :: [DailyWtime] -> IO ()
prettyPrintWtime = render . tableWtime

tableWtime :: [DailyWtime] -> [[Cell]]
tableWtime wtime = head : body ++ foot
 where
  prettyPrint (date, wtime) = [date, humanReadableDuration $ realToFrac wtime]
  head = tableWtimeHead
  body = map tableWtimeRow wtime
  foot = [tableWtimeFoot $ foldl (\acc (_, x) -> acc + x) 0 wtime]

tableWtimeHead :: [Cell]
tableWtimeHead = map (underline . bold . cell) ["DATE", "WORKTIME"]

tableWtimeRow :: DailyWtime -> [Cell]
tableWtimeRow wtime = cells
 where
  [date, total] = wtimeToStrings wtime
  cells         = [cell date, yellow . cell $ total]

tableWtimeFoot :: Duration -> [Cell]
tableWtimeFoot total = [bold . cell $ "TOTAL", bold . cell $ humanTotal]
  where humanTotal = humanReadableDuration total

wtimeToStrings :: DailyWtime -> [String]
wtimeToStrings (date, wtime) = [date, humanReadableDuration $ realToFrac wtime]
