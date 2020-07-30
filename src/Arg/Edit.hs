module Arg.Edit (EditOpts (EditOpts)) where

import Task (Id)

data EditOpts = EditOpts
  { id :: Id,
    desc :: String
  }
  deriving (Show)
