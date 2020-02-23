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

data Task = Task { _id :: Id
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

data WtimeTask = WtimeTask Ref Desc Duration deriving (Show, Read, Eq)
type DailyWtime = (String, [WtimeTask])
newtype DailyWtimeRecord = DailyWtimeRecord {toDailyWtimeRecord :: DailyWtime}
newtype DailyWtimeTotalRecord = DailyWtimeTotalRecord {toDailyWtimeTotalRecord :: Duration}

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
  toJSON (DailyWtimeRecord (date, _)) =
    object ["date" .= date, "wtime" .= DurationRecord 0]

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

withMinMax :: Maybe UTCTime -> Maybe UTCTime -> [UTCTime] -> [UTCTime]
withMinMax maybeMin maybeMax = map withMinMax'
 where
  withMinMax' date =
    let max = minimum $ [fromMaybe date maybeMax, date]
    in  maximum $ [fromMaybe max maybeMin, max]

mergeWtimes :: [DailyWtime] -> [DailyWtime] -> [DailyWtime]
mergeWtimes = foldl mergeWtimes'
 where
  mergeWtimes' a (bday, bvals) = case lookup bday a of
    Nothing -> (bday, bvals) : a
    Just avals ->
      (bday, foldl mergeWtimesVals avals bvals) : filter ((/=) bday . fst) a

mergeWtimesVals :: [WtimeTask] -> WtimeTask -> [WtimeTask]
mergeWtimesVals avals (WtimeTask bid bdesc bwtime) =
  case find ((==) bid . getId) avals of
    Nothing -> (WtimeTask bid bdesc bwtime) : avals
    Just aval ->
      (WtimeTask bid bdesc (getWtime aval + bwtime)) : without bid avals
 where
  getId (WtimeTask id desc wtime) = id
  getWtime (WtimeTask id desc wtime) = wtime
  without val = filter ((/=) val . getId)

getWtimePerDay
  :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> [Task] -> [DailyWtime]
getWtimePerDay now min max = foldl (getWtimePerDay' now min max) []
 where
  getWtimePerDay' now min max wtimes task = nextWtimes
   where
    wtimeTask = WtimeTask (_id task) (_desc task) 0
    starts    = withMinMax min max $ _starts task
    stops     = withMinMax min max $ _stops task ++ [ now | _active task > 0 ]
    getWtime (WtimeTask _ _ w) = w
    nextWtimes =
      filter (\(_, wtimes) -> (sum $ map getWtime wtimes) > 0)
        $ mergeWtimes wtimes
        $ concatMap (wtimePerDay wtimeTask)
        $ zip starts stops

wtimePerDay :: WtimeTask -> (UTCTime, UTCTime) -> [DailyWtime]
wtimePerDay (WtimeTask id desc _) (start, stop) = nextWtimes
 where
  day        = show currDay
  currDay    = utctDay start
  endOfDay   = read $ show currDay ++ " 23:59:59.999999999" :: UTCTime
  nextDay    = read $ show (addDays 1 currDay) ++ " 00:00:00" :: UTCTime
  nextWtimes = if stop < endOfDay
    then [(day, [WtimeTask id desc $ realToFrac $ diffUTCTime stop start])]
    else
      (day, [WtimeTask id desc $ realToFrac $ diffUTCTime endOfDay start])
        : wtimePerDay (WtimeTask id desc 0) (nextDay, stop)

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

prettyPrintTask :: Task -> IO ()
prettyPrintTask = render . tableTask

tableTask :: Task -> [[Cell]]
tableTask task = head : body
 where
  head = map (bold . underline . cell) ["KEY", "VALUE"]
  keys = map cell ["ID", "DESC", "TAGS", "ACTIVE", "DUE", "WORKTIME"]
  values =
    [ red . cell $ show $ _id task
    , cell $ _desc task
    , blue . cell $ unwords $ _tags task
    , green . cell $ printActive $ _active task
    , (if fromMaybe 0 (_due task) < 0 then bgRed . white else yellow)
      . cell
      $ printHumanTime
      $ _due task
    , yellow . cell $ humanReadableDuration $ _wtime task
    ]
  body = transpose [keys, values]

prettyPrintReport :: [Task] -> IO ()
prettyPrintReport = render . tableReport

tableReport :: [Task] -> [[Cell]]
tableReport tasks = head : body ++ foot
 where
  head = tableTaskHead
  body = map tableReportRow tasks
  foot = [tableReportFoot $ sum $ map _wtime tasks]

tableReportHead :: [Cell]
tableReportHead = map (bold . underline . cell)
                      ["ID", "DESC", "TAGS", "ACTIVE", "DUE", "WORKTIME"]

tableReportRow :: Task -> [Cell]
tableReportRow task =
  [ red . cell $ show $ _id task
  , cell $ _desc task
  , blue . cell $ unwords $ _tags task
  , green . cell $ printActive $ _active task
  , (if fromMaybe 0 (_due task) < 0 then bgRed . white else yellow)
    . cell
    $ printHumanTime
    $ _due task
  , yellow . cell $ humanReadableDuration (_wtime task)
  ]

tableReportFoot :: Duration -> [Cell]
tableReportFoot total =
  (bold . cell) "TOTAL"
    :  replicate 4 (cell "")
    ++ [bold . yellow . cell $ humanReadableDuration total]

prettyPrintTasks :: [Task] -> IO ()
prettyPrintTasks = render . tableTasks

tableTasks :: [Task] -> [[Cell]]
tableTasks tasks = head : body
 where
  head = tableTaskHead
  body = map tableTaskRow tasks

tableTaskHead :: [Cell]
tableTaskHead = map (bold . underline . cell)
                    ["ID", "DESC", "TAGS", "ACTIVE", "DUE", "WORKTIME"]

tableTaskRow :: Task -> [Cell]
tableTaskRow task =
  [ red . cell $ show $ _id task
  , cell $ _desc task
  , blue . cell $ unwords $ _tags task
  , green . cell $ printActive $ _active task
  , (if fromMaybe 0 (_due task) < 0 then bgRed . white else yellow)
    . cell
    $ printApproxTime
    $ _due task
  , yellow . cell $ if _wtime task > 0
    then approximativeDuration (_wtime task)
    else ""
  ]

prettyPrintWtime :: [DailyWtime] -> IO ()
prettyPrintWtime = render . tableWtime

tableWtime :: [DailyWtime] -> [[Cell]]
tableWtime wtime = head : body ++ foot
 where
  getWtime (WtimeTask _ _ w) = w
  head = tableWtimeHead
  body = map tableWtimeRow wtime
  foot = tableWtimeFoot $ sum $ map getWtime $ concatMap snd wtime

tableWtimeHead :: [Cell]
tableWtimeHead = map (underline . bold . cell) ["DATE", "WORKTIME"]

tableWtimeRow :: DailyWtime -> [Cell]
tableWtimeRow wtime = [cell $ fst wtime, yellow . cell $ total]
 where
  getWtime (WtimeTask _ _ wtime) = wtime
  total = humanReadableDuration $ sum $ map getWtime $ snd wtime

tableWtimeFoot :: Duration -> [[Cell]]
tableWtimeFoot total =
  [ replicate 2 $ ext 8 . cell $ replicate 3 '-'
  , [bold . cell $ "TOTAL RAW", bold . cell $ humanReadableDuration total]
  , [ bold . cell $ "TOTAL WDAY"
    , bold . cell $ humanReadableDuration (total * 3.2)
    ]
  ]

prettyPrintFullWtime :: [DailyWtime] -> IO ()
prettyPrintFullWtime = render . tableFullWtime

tableFullWtime :: [DailyWtime] -> [[Cell]]
tableFullWtime wtime = head : body ++ foot
 where
  head = tableFullWtimeHead
  body = concatMap tableFullWtimeRow wtime
  getWtime (WtimeTask _ _ w) = w
  foot = tableFullWtimeFoot $ sum $ map getWtime $ concatMap snd wtime

tableFullWtimeHead :: [Cell]
tableFullWtimeHead =
  map (underline . bold . cell) ["DATE", "ID", "DESC", "WORKTIME"]

tableFullWtimeRow :: DailyWtime -> [[Cell]]
tableFullWtimeRow wtime = map toCell (wtimeToStrings wtime) ++ [foot]
 where
  toCell [date, id, desc, total] =
    [ext 8 . cell $ date, red . cell $ id, cell desc, ext 8 . cell $ total]
  getWtime (WtimeTask _ _ wtime) = wtime
  total = humanReadableDuration $ sum $ map getWtime $ snd wtime
  foot  = [cell $ fst wtime, cell "", cell "", yellow . cell $ total]

tableFullWtimeFoot :: Duration -> [[Cell]]
tableFullWtimeFoot total =
  [ replicate 4 $ ext 8 . cell $ replicate 3 '-'
  , [ bold . cell $ "TOTAL RAW"
    , cell ""
    , cell ""
    , bold . cell $ humanReadableDuration total
    ]
  , [ bold . cell $ "TOTAL WDAY"
    , cell ""
    , cell ""
    , bold . cell $ humanReadableDuration (total * 3.2)
    ]
  ]

wtimeToStrings :: DailyWtime -> [[String]]
wtimeToStrings (date, tasks) = map wtimeToStrings' tasks
 where
  wtimeToStrings' (WtimeTask id desc wtime) =
    [date, show id, desc, humanReadableDuration $ realToFrac wtime]
