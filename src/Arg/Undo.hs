module Arg.Undo (UndoOpts (UndoOpts)) where

import Task (Id)

data UndoOpts = UndoOpts
  { ids :: [Id]
  }
  deriving (Show)
