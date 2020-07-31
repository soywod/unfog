module Arg.Do (DoOpts (DoOpts)) where

import Task (Id)

data DoOpts = DoOpts
  { ids :: [Id]
  }
  deriving (Show)
