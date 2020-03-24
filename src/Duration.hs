module Duration where

import           Prelude                 hiding ( min )
import           Data.Fixed

-- Durations

type Duration = Micro

ms :: Duration
ms = MkFixed 1000

sec :: Duration
sec = 1000 * ms

min :: Duration
min = 60 * sec

hour :: Duration
hour = 60 * min

day :: Duration
day = 24 * hour

year :: Duration
year = 365 * day

-- Show helpers

showMins :: Duration -> String
showMins = showDuration unit . flip div' min
 where
  unit n | n > 1     = "mins"
         | otherwise = "min"

showHours :: Duration -> String
showHours = showDuration unit . flip div' hour
 where
  unit n | n > 1     = "hours"
         | otherwise = "hour"

showDays :: Duration -> String
showDays = showDuration unit . flip div' day
 where
  unit n | n > 1     = "days"
         | otherwise = "day"

showYears :: Duration -> String
showYears = showDuration unit . flip div' year
 where
  unit n | n > 1     = "years"
         | otherwise = "year"

showDuration :: (Integer -> String) -> Integer -> String
showDuration showUnit n = unwords [show n, showUnit n]

showFullDuration :: Duration -> String
showFullDuration d = (unwords . reverse . show d) []
 where
  show d ds | d >= year = show (mod' d year) $ showYears d : ds
            | d >= day  = show (mod' d day) $ showDays d : ds
            | d >= hour = show (mod' d hour) $ showHours d : ds
            | d >= min  = show (mod' d min) $ showMins d : ds
            | otherwise = ds

showApproxDuration :: Duration -> String
showApproxDuration d | d >= year = showYears d
                     | d >= day  = showDays d
                     | d >= hour = showHours d
                     | d >= min  = showMins d
                     | otherwise = ""

showFullRelDuration :: Duration -> String
showFullRelDuration d | abs d < min = ""
                      | d < 0       = unwords [showFullDuration (abs d), "ago"]
                      | d > 0       = unwords ["in", showFullDuration (abs d)]
                      | otherwise   = ""

showApproxRelDuration :: Duration -> String
showApproxRelDuration d | abs d < min = ""
                        | d < 0 = unwords [showApproxDuration (abs d), "ago"]
                        | d > 0 = unwords ["in", showApproxDuration (abs d)]
                        | otherwise = ""
