module Unfog.Event.Type where

import Data.List (sortOn)
import Data.Time (UTCTime)
import Text.Read (readMaybe)
import Unfog.Task (Desc, Due, Id, Project)

data Event
  = TaskAdded UTCTime Id Desc Project Due
  | TaskEdited UTCTime Id Desc Project Due
  | TaskStarted UTCTime Id
  | TaskStopped UTCTime Id
  | TaskDid UTCTime Id
  | TaskUndid UTCTime Id
  | TaskDeleted UTCTime Id
  | TaskUndeleted UTCTime Id
  | ContextEdited UTCTime Project
  deriving (Show, Read, Eq)

readEvents :: (Read a) => String -> Maybe [a] -> Maybe [a]
readEvents _ Nothing = Nothing
readEvents line (Just evts) = (++ evts) . pure <$> readMaybe line

sortOnUTCTime :: [Event] -> [Event]
sortOnUTCTime = sortOn time
  where
    time (TaskAdded t _ _ _ _) = t
    time (TaskEdited t _ _ _ _) = t
    time (TaskStarted t _) = t
    time (TaskStopped t _) = t
    time (TaskDid t _) = t
    time (TaskUndid t _) = t
    time (TaskDeleted t _) = t
    time (TaskUndeleted t _) = t
    time (ContextEdited t _) = t
