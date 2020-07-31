module Arg.Worktime (WtimeOpts (WtimeOpts)) where

import Data.Time (UTCTime)
import Task (Tag)

data WtimeOpts = WtimeOpts
  { tags :: [Tag],
    from :: Maybe UTCTime,
    to :: Maybe UTCTime,
    moreOpt :: Bool,
    jsonOpt :: Bool
  }
  deriving (Show)
