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

applyAll :: [Event] -> State
applyAll = foldl apply emptyState
  where emptyState = State { _tasks = [], _showDone = False, _context = [] }

apply :: State -> Event -> State
apply state event = case event of
  TaskAdded _ _id _number _desc _tags -> state { _tasks = nextTasks }
   where
    prevTasks = _tasks state
    newTask   = emptyTask { _id, _number, _desc, _tags }
    nextTasks = prevTasks ++ [newTask]

  TaskEdited _ id _ _desc _tags -> state { _tasks = nextTasks }
   where
    maybeTask = findById id $ filterByDone (_showDone state) (_tasks state)
    nextTasks = case maybeTask of
      Nothing   -> _tasks state
      Just task -> map updateTask $ _tasks state
       where
        nextTask = task { _desc, _tags }
        updateTask currTask | _id currTask == _id nextTask = nextTask
                            | otherwise                    = currTask

  TaskStarted _ id _ -> state { _tasks = nextTasks }
   where
    maybeTask = findById id $ filterByDone (_showDone state) (_tasks state)
    nextTasks = case maybeTask of
      Nothing   -> _tasks state
      Just task -> map updateTask $ _tasks state
       where
        nextTask = task { _active = True }
        updateTask currTask | _id currTask == _id nextTask = nextTask
                            | otherwise                    = currTask

  TaskStopped _ id _ -> state { _tasks = nextTasks }
   where
    maybeTask = findById id $ filterByDone (_showDone state) (_tasks state)
    nextTasks = case maybeTask of
      Nothing   -> _tasks state
      Just task -> map updateTask $ _tasks state
       where
        nextTask = task { _active = False }
        updateTask currTask | _id currTask == _id nextTask = nextTask
                            | otherwise                    = currTask

  TaskMarkedAsDone _ id _number -> state { _tasks = nextTasks }
   where
    maybeTask = findById id $ filterByDone False (_tasks state)
    nextTasks = case maybeTask of
      Nothing   -> _tasks state
      Just task -> map updateTask $ _tasks state
       where
        nextTask = task { _number, _active = False, _done = True }
        updateTask currTask | _id currTask == id = nextTask
                            | otherwise          = currTask

  TaskDeleted _ id _ -> state { _tasks = nextTasks }
   where
    maybeTask = findById id $ filterByDone (_showDone state) (_tasks state)
    nextTasks = case maybeTask of
      Nothing   -> _tasks state
      Just task -> filter ((/=) (_id task) . _id) (_tasks state)

  ContextSet _ _showDone _context -> state { _showDone, _context }
