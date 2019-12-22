{-# LANGUAGE NamedFieldPuns #-}
module State where

import           Data.Maybe
import           Data.List
import           Data.Time

import           Event
import           Task
import           Utils

data State = State { _tasks :: [Task]
                   , _ctx :: [Tag]
                   } deriving (Show, Read)

applyEvents :: UTCTime -> [Event] -> State
applyEvents now = foldl (apply now) emptyState
  where emptyState = State { _tasks = [], _ctx = [] }

apply :: UTCTime -> State -> Event -> State
apply now state event = case event of
  TaskCreated t _ref _id _pos _desc _tags due -> state { _tasks = nextTasks }
   where
    nextTasks = _tasks state ++ [newTask]
    newTask =
      emptyTask { _ref, _id, _pos, _desc, _tags, _due = due, _createdAt = t }

  TaskUpdated _ ref _ _pos _desc _tags due -> state { _tasks = nextTasks }
   where
    update task = task { _pos, _desc, _tags, _due = due }
    nextTasks = getNextTasks ref update

  TaskStarted start ref _ -> state { _tasks = getNextTasks ref update }
   where
    active = realToFrac $ diffUTCTime now start
    update task = task { _active = active, _starts = _starts task ++ [start] }

  TaskStopped stop ref _ -> state { _tasks = getNextTasks ref update }
    where update task = task { _active = 0, _stops = _stops task ++ [stop] }

  TaskMarkedAsDone stop ref _id -> state { _tasks = getNextTasks ref update }
   where
    update task =
      let nextStops = _stops task ++ [ stop | _active task > 0 ]
      in  task { _id, _active = 0, _done = True, _stops = nextStops }

  TaskDeleted _ ref _ -> state { _tasks = nextTasks }
    where nextTasks = filter ((/=) ref . _ref) (_tasks state)

  ContextSet _ _ctx -> state { _ctx }

 where
  getNextTasks ref update =
    case findByRef ref $ filterByTags (_ctx state) (_tasks state) of
      Nothing -> _tasks state
      Just task ->
        let nextTask = update task
            save currTask | _ref currTask == _ref nextTask = nextTask
                          | otherwise                      = currTask
        in  map save (_tasks state)
