module Arg.Undo (UndoOpts (UndoOpts)) where

import Task (Id)

data UndoOpts = UndoOpts
  { id :: Id
  }
  deriving (Show)
