module Event where

import           Data.Time

import           Task

data Event
  = TaskCreated UTCTime Ref Id Pos Desc [Tag] (Maybe Duration)
  | TaskUpdated UTCTime Ref Id Pos Desc [Tag] (Maybe Duration)
  | TaskStarted UTCTime Ref Id
  | TaskStopped UTCTime Ref Id
  | TaskMarkedAsDone UTCTime Ref Id
  | TaskDeleted UTCTime Ref Id
  | ContextSet UTCTime [Tag]
  deriving (Show, Read)
