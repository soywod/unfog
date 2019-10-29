module Event where

import           Data.Time

import           Task

data Event
  = TaskAdded UTCTime Id Desc [Tag]
  | TaskEdited UTCTime Id Desc [Tag]
  | TaskStarted UTCTime Id
  | TaskStopped UTCTime Id
  | TaskMarkedAsDone UTCTime Id Id
  deriving (Show, Read)
