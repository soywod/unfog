module Worktime where

import ArgOptions
import Data.List
import Data.Maybe
import Data.Time
import Duration
import Task

data Worktime = Worktime
  { getWtimeId :: Id,
    getWtimeDesc :: Desc,
    getWtimeDuration :: Duration
  }
  deriving (Show, Read, Eq)

type DailyWorktime = (String, [Worktime])

clampTime :: FromOpt -> ToOpt -> UTCTime -> UTCTime
clampTime from to date = maximum [fromMaybe to' from, to']
  where
    to' = minimum [fromMaybe date to, date]

getTaskWtime :: UTCTime -> Task -> Duration
getTaskWtime now task = realToFrac $ sum $ zipWith diffUTCTime stops starts
  where
    starts = getStarts task
    stops = getStops task ++ [now | not $ isNothing $ getActive task]

buildWtimePerDay :: UTCTime -> FromOpt -> ToOpt -> [Task] -> [DailyWorktime]
buildWtimePerDay now from to = reverse . sortOn fst . foldl buildWtimePerDay' []
  where
    buildWtimePerDay' wtimes task = filter (\(_, wtimes) -> (sum $ map getWtimeDuration wtimes) > 0) $ mergeDailyWtimes wtimes $ concatMap (wtimePerDay wtime) $ zip starts' stops'
      where
        wtime = Worktime (getId task) (getDesc task) 0
        starts' = map (clampTime from to) $ getStarts task
        stops' = map (clampTime from to) $ getStops task ++ [now | isJust $ getActive task]

wtimePerDay :: Worktime -> (UTCTime, UTCTime) -> [DailyWorktime]
wtimePerDay (Worktime id desc _) (start, stop)
  | stop < endOfDay = [(day, [Worktime id desc $ realToFrac $ diffUTCTime stop start])]
  | otherwise = (day, [Worktime id desc $ realToFrac $ diffUTCTime endOfDay start]) : wtimePerDay (Worktime id desc 0) (nextDay, stop)
  where
    day = show currDay
    currDay = utctDay start
    endOfDay = read $ show currDay ++ " 23:59:59 UTC" :: UTCTime
    nextDay = read $ show (addDays 1 currDay) ++ " 00:00:00 UTC" :: UTCTime

mergeDailyWtimes :: [DailyWorktime] -> [DailyWorktime] -> [DailyWorktime]
mergeDailyWtimes = foldl mergeWtimes'
  where
    mergeWtimes' a (bday, bvals) = case lookup bday a of
      Nothing -> (bday, bvals) : a
      Just avals ->
        (bday, foldl mergeWtimes avals bvals) : filter ((/=) bday . fst) a

mergeWtimes :: [Worktime] -> Worktime -> [Worktime]
mergeWtimes xvals (Worktime yid ydesc ywtime) =
  case find ((==) yid . getWtimeId) xvals of
    Nothing -> (Worktime yid ydesc ywtime) : xvals
    Just aval -> (Worktime yid ydesc (getWtimeDuration aval + ywtime)) : without yid xvals
  where
    without val = filter ((/=) val . getWtimeId)
