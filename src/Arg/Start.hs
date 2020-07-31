module Arg.Start (StartOpts (StartOpts)) where

import Task (Id)

data StartOpts = StartOpts
  { ids :: [Id]
  }
  deriving (Show)
