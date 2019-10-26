module State where

import           Event
import           Task

newtype State = State { tasks :: [Task]
                      } deriving (Show, Read)

apply :: [Event] -> State
apply = foldl apply' state where state = State { tasks = [] }

apply' :: State -> Event -> State
apply' state event = case event of
  TaskAdded task -> state { tasks = tasks state ++ [task] }
