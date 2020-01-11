module DailyWtime where

import           Task

type TasksWithTotalWtime = ([Task], Duration)

data DailyWtime =
  DailyWtime { _day :: String
             , _tasks :: TasksWithTotalWtime
             , _total :: Duration
             } deriving (Show, Read, Eq)
