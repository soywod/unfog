module Event where

import           Data.Time

import           Task

data Event
  = TaskCreated UTCTime Ref Id Pos Desc [Tag] Due
  | TaskUpdated UTCTime Ref Id Pos Desc [Tag] Due
  | TaskStarted UTCTime Ref Id
  | TaskStopped UTCTime Ref Id
  | TaskMarkedAsDone UTCTime Ref Id
  | TaskDeleted UTCTime Ref Id
  | ContextSet UTCTime Done [String]
  deriving (Show, Read)
