module State where

import           Event
import           Task

newtype State = State { _tasks :: [Task]
                      } deriving (Show, Read)

applyAll :: [Event] -> State
applyAll = foldl apply emptyState where emptyState = State { _tasks = [] }

apply :: State -> Event -> State
apply state event = case event of
  TaskAdded  _ newTask  -> state { _tasks = _tasks state ++ [newTask] }
  TaskEdited _ nextTask -> state { _tasks = map updateTask $ _tasks state }
   where
    updateTask currTask | _id currTask == _id nextTask = nextTask
                        | otherwise                    = currTask
