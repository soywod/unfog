module Event where

import           Data.Time

import           Task

data Event
  = TaskCreated UTCTime Ref Id Pos Desc [Tag] (Maybe UTCTime)
  | TaskUpdated UTCTime Ref Id Pos Desc [Tag] (Maybe UTCTime)
  | TaskStarted UTCTime Ref Id
  | TaskStopped UTCTime Ref Id
  | TaskMarkedAsDone UTCTime Ref Id
  | TaskDeleted UTCTime Ref Id
  | ContextSet UTCTime [Tag]
  deriving (Show, Read)
