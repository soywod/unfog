module Arg.Delete (DeleteOpts (DeleteOpts)) where

import Task (Id)

data DeleteOpts = DeleteOpts
  { ids :: [Id]
  }
  deriving (Show)
