module Arg.Info (InfoOpts (InfoOpts)) where

import Task (Id)

data InfoOpts = InfoOpts
  { id :: Id,
    jsonOpt :: Bool
  }
  deriving (Show)
