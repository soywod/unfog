module Arg.Do (DoOpts (DoOpts)) where

import Task (Id)

data DoOpts = DoOpts
  { id :: Id
  }
  deriving (Show)
