{-# LANGUAGE NamedFieldPuns #-}
module State where

import           Data.List
import           Data.Maybe
import           Data.Time

import           Event
import           Task

data State = State { _tasks :: [Task]
                   , _ctx :: [Tag]
                   } deriving (Show, Read)

applyEvents :: UTCTime -> [Event] -> State
applyEvents now = foldl (apply now) emptyState
  where emptyState = State { _tasks = [], _ctx = [] }

apply :: UTCTime -> State -> Event -> State
apply now state event = case event of
  TaskCreated _ _ref _id _pos _desc _tags due -> state { _tasks = nextTasks }
   where
    due'      = realToFrac <$> flip diffUTCTime now <$> due
    newTask   = emptyTask { _ref, _id, _pos, _desc, _tags, _due = due' }
    nextTasks = _tasks state ++ [newTask]

  TaskUpdated _ ref _ _pos _desc _tags due -> state { _tasks = nextTasks }
   where
    due' = realToFrac <$> diffUTCTime now <$> due
    update task = task { _pos, _desc, _tags, _due = due' }
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

  TaskUnmarkedAsDone stop ref _id -> state { _tasks = getNextTasks ref update }
    where update task = task { _id, _done = False }

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
