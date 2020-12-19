module Unfog.Command.Types where

import Data.Time (UTCTime)
import Unfog.Response (ResponseType)
import Unfog.Task (Desc, Due, Id, Project, ShortId)

data Command
  = AddTask UTCTime ResponseType Id Desc Project Due
  | EditTask UTCTime ResponseType Id Desc Project Due
  | StartTask UTCTime ResponseType Id
  | StopTask UTCTime ResponseType Id
  | DoTask UTCTime ResponseType Id
  | UndoTask UTCTime ResponseType Id
  | DeleteTask UTCTime ResponseType Id
  | UndeleteTask UTCTime ResponseType Id
  | EditContext UTCTime ResponseType Project
  | Error ResponseType String
  deriving (Show, Read, Eq)
