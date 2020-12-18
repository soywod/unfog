module Unfog.Arg.Types where

import Data.Time (UTCTime)
import Unfog.Task (Desc, Due, Id, Project)

data Query
  = ShowTasks DueInOpt DoneOpt DeletedOpt JsonOpt
  | ShowTask Id JsonOpt
  | ShowWorktime Project FromOpt ToOpt MoreOpt JsonOpt
  | ShowStatus MoreOpt JsonOpt
  deriving (Show, Eq)

data Command
  = AddTask Desc ProjOpt DueOpt JsonOpt
  | EditTask Id Desc ProjOpt DueOpt JsonOpt
  | StartTask [Id] JsonOpt
  | StopTask [Id] JsonOpt
  | ToggleTask [Id] JsonOpt
  | DoTask [Id] JsonOpt
  | UndoTask [Id] JsonOpt
  | DeleteTask [Id] JsonOpt
  | UndeleteTask [Id] JsonOpt
  | EditContext Project JsonOpt
  deriving (Show, Eq)

data Procedure
  = ShowVersion JsonOpt
  | Upgrade
  | ClearCache

data Arg
  = CommandArg Command
  | QueryArg Query
  | ProcedureArg Procedure

type ProjOpt = Project

type DueOpt = Due

type DueInOpt = Maybe Int

type FromOpt = Maybe UTCTime

type ToOpt = Maybe UTCTime

type DoneOpt = Bool

type DeletedOpt = Bool

type OnlyProjsOpt = Bool

type OnlyIdsOpt = Bool

type VersionOpt = Bool

type MoreOpt = Bool

type JsonOpt = Bool
