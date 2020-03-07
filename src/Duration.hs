module Duration where

import qualified Data.Duration                 as D
import           Data.Fixed

type Duration = D.Seconds

showDuration :: Duration -> String
showDuration d = (unwords . reverse . showDuration' d) []
 where
  showUnit d unit = unwords [show d, if d > 1 then unit else init unit]
  showDuration' d ds
    | d > D.year
    = showDuration' (mod' d D.year) $ showUnit (D.getYears d) "years" : ds
    | d > D.day
    = showDuration' (mod' d D.day) $ showUnit (D.getDays d) "days" : ds
    | d > D.hour
    = showDuration' (mod' d D.hour) $ showUnit (D.getHours d) "hours" : ds
    | d > D.minute
    = showDuration' (mod' d D.minute) $ showUnit (D.getMinutes d) "mins" : ds
    | otherwise
    = ds

showDurationApprox :: Duration -> String
showDurationApprox = D.approximativeDuration

showRelativeDuration :: Duration -> String
showRelativeDuration d | d < 0     = showDuration (abs d) ++ " ago"
                       | d > 0     = "in " ++ showDuration (abs d)
                       | otherwise = ""

showRelativeDurationApprox :: Duration -> String
showRelativeDurationApprox d | d < 0     = showDurationApprox (abs d) ++ " ago"
                             | d > 0     = "in " ++ showDurationApprox (abs d)
                             | otherwise = ""
