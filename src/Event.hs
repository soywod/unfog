module Event where

import           Data.Time

import           Task

data Event
  = TaskCreated UTCTime Id Number Desc [Tag]
  | TaskUpdated UTCTime Id Number Desc [Tag]
  | TaskReplaced UTCTime Id Number Desc [Tag]
  | TaskStarted UTCTime Id Number
  | TaskStopped UTCTime Id Number
  | TaskMarkedAsDone UTCTime Id Number
  | TaskDeleted UTCTime Id Number
  | ContextSet UTCTime Bool [String]
  deriving (Show, Read)
