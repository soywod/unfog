module Unfog.ArgOptions where

import Data.Time (UTCTime)
import Unfog.Task (Due, Project)

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
