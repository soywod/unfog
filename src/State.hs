{-# LANGUAGE NamedFieldPuns #-}
module State where

import           Data.Maybe
import           Data.List

import           Event
import           Task
import           Utils

data State = State { _tasks :: [Task]
                   , _showDone :: Bool
                   , _context :: [Tag]
                   } deriving (Show, Read)

applyEvents :: [Event] -> State
applyEvents = foldl apply emptyState
  where emptyState = State { _tasks = [], _showDone = False, _context = [] }

apply :: State -> Event -> State
apply state event = case event of
  TaskCreated _ _ref _id _pos _desc _tags due -> state { _tasks = nextTasks }
   where
    newTask   = emptyTask { _ref, _id, _pos, _desc, _tags, _due = Just due }
    nextTasks = _tasks state ++ [newTask]

  TaskUpdated _ ref _ _pos _desc _tags due -> state { _tasks = nextTasks }
   where
    update task = task { _pos, _desc, _tags, _due = Just due }
    nextTasks = getNextTasks ref update

  TaskStarted start ref _ -> state { _tasks = getNextTasks ref update }
   where
    update task = task { _active = True, _starts = _starts task ++ [start] }

  TaskStopped stop ref _ -> state { _tasks = getNextTasks ref update }
   where
    update task = task { _active = False, _stops = _stops task ++ [stop] }

  TaskMarkedAsDone stop ref _id -> state { _tasks = getNextTasks ref update }
   where
    update task =
      let nextStops = _stops task ++ [ stop | _active task ]
      in  task { _id, _active = False, _done = True, _stops = nextStops }

  TaskDeleted _ ref _ -> state { _tasks = nextTasks }
   where
    update _ = emptyTask
    nextTasks = filter (emptyTask /=) $ getNextTasks ref update

  ContextSet _ _showDone _context -> state { _showDone, _context }

 where
  getNextTasks ref update =
    case findByRef ref $ filterByDone (_showDone state) (_tasks state) of
      Nothing -> _tasks state
      Just task ->
        let nextTask = update task
            save currTask | _ref currTask == _ref nextTask = nextTask
                          | otherwise                      = currTask
        in  map save (_tasks state)
