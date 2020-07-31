module Arg.Stop (StopOpts (StopOpts)) where

import Task (Id)

data StopOpts = StopOpts
  { ids :: [Id]
  }
  deriving (Show)
