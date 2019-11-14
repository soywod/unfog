module Event where

import           Data.Time

import           Task

data Event
  = TaskCreated UTCTime Reference Id Position Description [Tag] Due
  | TaskUpdated UTCTime Reference Id Position Description [Tag] Due
  | TaskStarted UTCTime Reference Id
  | TaskStopped UTCTime Reference Id
  | TaskMarkedAsDone UTCTime Reference Id
  | TaskDeleted UTCTime Reference Id
  | ContextSet UTCTime Done [String]
  deriving (Show, Read)
