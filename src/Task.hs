{-# LANGUAGE OverloadedStrings #-}

module Task where

import Data.Aeson
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Time
import Data.UUID
import Duration
import Table

type Id = String

type Pos = Int

type Desc = String

type Tag = String

type Active = Maybe UTCTime

type Done = Bool

data Task = Task
  { getId :: Id,
    getDesc :: Desc,
    getTags :: [Tag],
    getActive :: Active,
    getDone :: Done,
    getStarts :: [UTCTime],
    getStops :: [UTCTime]
  }
  deriving (Show, Read, Eq)

findById :: Id -> [Task] -> Maybe Task
findById id = find $ isPrefixOf id . getId

-- data WtimeTask = WtimeTask Desc Duration deriving (Show, Read, Eq)
-- type DailyWtime = (String, [WtimeTask])
-- newtype DailyWtimeRecord = DailyWtimeRecord {toDailyWtimeRecord :: DailyWtime}
-- newtype DailyWtimeTotalRecord = DailyWtimeTotalRecord {toDailyWtimeTotalRecord :: Duration}

-- newtype DurationRecord = DurationRecord {toWtimeRecord :: Duration}
-- instance ToJSON DurationRecord where
--   toJSON (DurationRecord wtime) = object
--     [ "approx" .= showApproxDuration wtime
--     , "human" .= showFullDuration wtime
--     , "micro" .= wtime
--     ]

-- instance ToJSON Task where
--   toJSON (Task id ref pos desc tags due active done wtime _ _) = object
--     [ "id" .= id
--     , "ref" .= ref
--     , "pos" .= pos
--     , "desc" .= desc
--     , "tags" .= tags
--     , "active" .= TimeRecord (Just (-active))
--     , "due" .= (TimeRecord due)
--     , "done" .= if done then 1 else 0 :: Int
--     , "wtime" .= DurationRecord wtime
--     ]

-- instance ToJSON DailyWtimeRecord where
--   toJSON (DailyWtimeRecord (date, wtime)) = object
--     ["date" .= date, "wtime" .= DurationRecord (sum $ map getWtime wtime)]

-- getWtime :: WtimeTask -> Duration
-- getWtime (WtimeTask _ _ w) = w

-- generateId :: [Task] -> Id
-- generateId tasks = generateId' (sort $ map id tasks) [1 ..]
--  where
--   generateId' [] []           = 1
--   generateId' [] (nextId : _) = nextId
--   generateId' (currId : currIds) (nextId : nextIds)
--     | currId == nextId = generateId' currIds nextIds
--     | otherwise        = nextId

-- findById :: Id -> [Task] -> Maybe Task
-- findById id = find $ (==) id . Task.id

-- findByRef :: Ref -> [Task] -> Maybe Task
-- findByRef ref = find $ (==) ref . Task.ref

-- filterByDone :: Done -> [Task] -> [Task]
-- filterByDone showDone tasks = filteredTasks
--  where
--   filteredTasks = filter byDone tasks
--   byDone        = if showDone then done else not . done

-- filterByIds :: [Id] -> [Task] -> [Task]
-- filterByIds ids = filter (flip elem ids . id)

-- filterByRefs :: [Ref] -> [Task] -> [Task]
-- filterByRefs refs = filter (flip elem refs . Task.ref)

-- filterByTags :: [Tag] -> [Task] -> [Task]
-- filterByTags tags tasks = filteredTasks
--  where
--   tags'         = tags \\ ["done"]
--   filteredTasks = if null tags' then tasks else filter byTags tasks
--   byTags        = not . null . intersect tags' . Task.tags

-- mapWithWtime :: UTCTime -> [Task] -> [Task]
-- mapWithWtime now = map withWtime
--   where withWtime task = task { wtime = getTotalWtime now task }

-- getTotalWtime :: UTCTime -> Task -> Duration
-- getTotalWtime now task = realToFrac $ sum $ zipWith diffUTCTime stops' starts'
--  where
--   starts' = starts task
--   stops'  = stops task ++ [ now | active task > 0 ]

-- withMinMax :: Maybe UTCTime -> Maybe UTCTime -> [UTCTime] -> [UTCTime]
-- withMinMax maybeMin maybeMax = map withMinMax'
--  where
--   withMinMax' date =
--     let max = minimum $ [fromMaybe date maybeMax, date]
--     in  maximum $ [fromMaybe max maybeMin, max]

-- mergeWtimes :: [DailyWtime] -> [DailyWtime] -> [DailyWtime]
-- mergeWtimes = foldl mergeWtimes'
--  where
--   mergeWtimes' a (bday, bvals) = case lookup bday a of
--     Nothing -> (bday, bvals) : a
--     Just avals ->
--       (bday, foldl mergeWtimesVals avals bvals) : filter ((/=) bday . fst) a

-- mergeWtimesVals :: [WtimeTask] -> WtimeTask -> [WtimeTask]
-- mergeWtimesVals avals (WtimeTask bid bdesc bwtime) =
--   case find ((==) bid . getId) avals of
--     Nothing -> (WtimeTask bid bdesc bwtime) : avals
--     Just aval ->
--       (WtimeTask bid bdesc (getWtime aval + bwtime)) : without bid avals
--  where
--   getId (WtimeTask id desc wtime) = id
--   without val = filter ((/=) val . getId)

-- getWtimePerDay
--   :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> [Task] -> [DailyWtime]
-- getWtimePerDay now min max =
--   reverse . sortOn fst . foldl (getWtimePerDay' now min max) []
--  where
--   getWtimePerDay' now min max wtimes task = nextWtimes
--    where
--     wtimeTask = WtimeTask (id task) (desc task) 0
--     starts'    = withMinMax min max $ starts task
--     stops'    = withMinMax min max $ stops task ++ [ now | active task > 0 ]
--     nextWtimes =
--       filter (\(_, wtimes) -> (sum $ map getWtime wtimes) > 0)
--         $ mergeWtimes wtimes
--         $ concatMap (wtimePerDay wtimeTask)
--         $ zip starts' stops'

-- wtimePerDay :: WtimeTask -> (UTCTime, UTCTime) -> [DailyWtime]
-- wtimePerDay (WtimeTask id desc _) (start, stop) = nextWtimes
--  where
--   day        = show currDay
--   currDay    = utctDay start
--   endOfDay   = read $ show currDay ++ " 23:59:59 UTC" :: UTCTime
--   nextDay    = read $ show (addDays 1 currDay) ++ " 00:00:00 UTC" :: UTCTime
--   nextWtimes = if stop < endOfDay
--     then [(day, [WtimeTask id desc $ realToFrac $ diffUTCTime stop start])]
--     else
--       (day, [WtimeTask id desc $ realToFrac $ diffUTCTime endOfDay start])
--         : wtimePerDay (WtimeTask id desc 0) (nextDay, stop)

-- printApproxTime :: Active -> String
-- printApproxTime Nothing    = ""
-- printApproxTime (Just due) = showApproxRelDuration due

-- printHumanTime :: Maybe Duration -> String
-- printHumanTime Nothing    = ""
-- printHumanTime (Just due) = showFullRelDuration due

-- prettyPrintTask :: Task -> IO ()
-- prettyPrintTask = render . tableTask

-- tableTask :: Task -> [[Cell]]
-- tableTask task = head : body
--  where
--   head = map (bold . underline . cell) ["KEY", "VALUE"]
--   keys = map cell ["ID", "DESC", "TAGS", "ACTIVE", "DUE", "WORKTIME"]
--   values =
--     [ red . cell $ show $ id task
--     , cell $ desc task
--     , blue . cell $ unwords $ tags task
--     , green . cell $ printActive $ active task
--     , (if fromMaybe 0 (due task) < 0 then bgRed . white else yellow)
--       . cell
--       $ printHumanTime
--       $ due task
--     , yellow . cell $ showFullDuration $ wtime task
--     ]
--   body = transpose [keys, values]

-- renderApproxRelActive :: UTCTime -> Active -> String
-- renderApproxRelActive _ Nothing = ""
-- renderApproxRelActive now (Just active) = showApproxRelDuration $ realToFrac $ diffUTCTime active now

-- renderTasksTable :: UTCTime -> [Task] -> String
-- renderTasksTable now tasks = render $ tableTasks now tasks

-- tableTasks :: UTCTime -> [Task] -> [[Cell]]
-- tableTasks now tasks = tableTaskHead : map (tableTaskRow now) tasks

-- tableTaskHead :: [Cell]
-- tableTaskHead = map (bold . underline . cell) ["ID", "DESC", "TAGS", "ACTIVE"]

-- tableTaskRow :: UTCTime -> Task -> [Cell]
-- tableTaskRow now task =
--   [ red $ cell $ getId task,
--     cell $ getDesc task,
--     blue $ cell $ unwords $ getTags task,
--     green $ cell $ renderApproxRelActive now $ getActive task
--   ]

-- prettyPrintTasks :: [Task] -> IO ()
-- prettyPrintTasks = render . tableTasks

-- tableTasks :: [Task] -> [[Cell]]
-- tableTasks tasks = head : body
--  where
--   head = tableTaskHead
--   body = map tableTaskRow tasks

-- tableTaskHead :: [Cell]
-- tableTaskHead = map (bold . underline . cell)
--                     ["ID", "DESC", "TAGS", "ACTIVE", "DUE", "WORKTIME"]

-- tableTaskRow :: Task -> [Cell]
-- tableTaskRow task =
--   [ red . cell $ show $ id task
--   , cell $ desc task
--   , blue . cell $ unwords $ tags task
--   , green . cell $ printActive $ active task
--   , (if fromMaybe 0 (due task) < 0 then bgRed . white else yellow)
--     . cell
--     $ printApproxTime
--     $ due task
--   , yellow . cell $ if wtime task > 0
--     then showApproxDuration (wtime task)
--     else ""
--   ]

-- prettyPrintWtime :: [DailyWtime] -> IO ()
-- prettyPrintWtime = render . tableWtime

-- tableWtime :: [DailyWtime] -> [[Cell]]
-- tableWtime wtime = head : body ++ foot
--  where
--   head = tableWtimeHead
--   body = map tableWtimeRow wtime
--   foot = tableWtimeFoot $ sum $ map getWtime $ concatMap snd wtime

-- tableWtimeHead :: [Cell]
-- tableWtimeHead = map (underline . bold . cell) ["DATE", "WORKTIME"]

-- tableWtimeRow :: DailyWtime -> [Cell]
-- tableWtimeRow wtime = [cell $ fst wtime, yellow . cell $ total]
--   where total = showFullDuration $ sum $ map getWtime $ snd wtime

-- tableWtimeFoot :: Duration -> [[Cell]]
-- tableWtimeFoot total =
--   [ replicate 2 $ ext 8 . cell $ replicate 3 '-'
--   , [bold . cell $ "TOTAL RAW", bold . cell $ showFullDuration total]
--   , [bold . cell $ "TOTAL WDAY", bold . cell $ showFullDuration (total * 3.2)]
--   ]

-- prettyPrintFullWtime :: [DailyWtime] -> IO ()
-- prettyPrintFullWtime = render . tableFullWtime

-- tableFullWtime :: [DailyWtime] -> [[Cell]]
-- tableFullWtime wtime = head : body ++ foot
--  where
--   head = tableFullWtimeHead
--   body = concatMap tableFullWtimeRow wtime
--   foot = tableFullWtimeFoot $ sum $ map getWtime $ concatMap snd wtime

-- tableFullWtimeHead :: [Cell]
-- tableFullWtimeHead =
--   map (underline . bold . cell) ["DATE", "ID", "DESC", "WORKTIME"]

-- tableFullWtimeRow :: DailyWtime -> [[Cell]]
-- tableFullWtimeRow wtime = map toCell (wtimeToStrings wtime) ++ [foot]
--  where
--   toCell [date, id, desc, total] =
--     [ext 8 . cell $ date, red . cell $ id, cell desc, ext 8 . cell $ total]
--   total = showFullDuration $ sum $ map getWtime $ snd wtime
--   foot  = [cell $ fst wtime, cell "", cell "", yellow . cell $ total]

-- tableFullWtimeFoot :: Duration -> [[Cell]]
-- tableFullWtimeFoot total =
--   [ replicate 4 $ ext 8 . cell $ replicate 3 '-'
--   , [ bold . cell $ "TOTAL RAW"
--     , cell ""
--     , cell ""
--     , bold . cell $ showFullDuration total
--     ]
--   , [ bold . cell $ "TOTAL WDAY"
--     , cell ""
--     , cell ""
--     , bold . cell $ showFullDuration (total * 3.2)
--     ]
--   ]

-- wtimeToStrings :: DailyWtime -> [[String]]
-- wtimeToStrings (date, tasks) = map wtimeToStrings' tasks
--  where
--   wtimeToStrings' (WtimeTask id desc wtime) =
--     [date, show id, desc, showFullDuration $ realToFrac wtime]
