module Arg.Stop (StopOpts (StopOpts)) where

import Task (Id)

data StopOpts = StopOpts
  { id :: Id
  }
  deriving (Show)
