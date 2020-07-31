module Arg.Start (StartOpts (StartOpts)) where

import Task (Id)

data StartOpts = StartOpts
  { id :: Id
  }
  deriving (Show)
