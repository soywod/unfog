module Unfog.Event.TypeV0 where

import Data.Time (UTCTime)

data EventV0
  = TaskCreated UTCTime Int Int Int String [String] (Maybe UTCTime)
  | TaskUpdated UTCTime Int Int Int String [String] (Maybe UTCTime)
  | TaskStarted UTCTime Int Int
  | TaskStopped UTCTime Int Int
  | TaskMarkedAsDone UTCTime Int Int
  | TaskUnmarkedAsDone UTCTime Int Int
  | TaskDeleted UTCTime Int Int
  | ContextSet UTCTime [String]
  deriving (Show, Read)
