module ArgOptions where

import Data.Time (UTCTime)
import Task (Due, Project)

type ProjOpt = Project

type DueOpt = Due

type FromOpt = Maybe UTCTime

type ToOpt = Maybe UTCTime

type OnlyProjsOpt = Bool

type OnlyIdsOpt = Bool

type VersionOpt = Bool

type MoreOpt = Bool

type JsonOpt = Bool
