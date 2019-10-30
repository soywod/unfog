{-# LANGUAGE NamedFieldPuns #-}
module State where

import           Data.Maybe
import           Data.List

import           Event
import           Task

newtype State = State { _tasks :: [Task]
                      } deriving (Show, Read)

applyAll :: [Event] -> State
applyAll = foldl apply emptyState where emptyState = State { _tasks = [] }

apply :: State -> Event -> State
apply state event = case event of
  TaskAdded _ _id _desc _tags -> state { _tasks = nextTasks }
   where
    prevTasks = _tasks state
    newTask   = emptyTask { _id, _desc, _tags }
    nextTasks = prevTasks ++ [newTask]

  TaskEdited _ id desc tags -> state { _tasks = nextTasks }
   where
    tasks     = filter (not . _done) (_tasks state)
    taskFound = Task.findById id tasks
    nextDesc  = if desc == "" then maybe desc _desc taskFound else desc
    nextTags  = maybe tags (union tags <$> _tags) taskFound
    nextTasks = case taskFound of
      Nothing   -> tasks
      Just task -> map updateTask tasks
       where
        nextTask = task { _desc = nextDesc, _tags = nextTags }
        updateTask currTask | _id currTask == _id nextTask = nextTask
                            | otherwise                    = currTask

  TaskStarted _ id -> state { _tasks = nextTasks }
   where
    tasks     = filter (not . _done) (_tasks state)
    taskFound = Task.findById id tasks
    nextTasks = case taskFound of
      Nothing   -> tasks
      Just task -> map updateTask tasks
       where
        nextTask = task { _active = True }
        updateTask currTask | _id currTask == _id nextTask = nextTask
                            | otherwise                    = currTask

  TaskStopped _ id -> state { _tasks = nextTasks }
   where
    tasks     = filter (not . _done) (_tasks state)
    taskFound = Task.findById id tasks
    nextTasks = case taskFound of
      Nothing   -> tasks
      Just task -> map updateTask tasks
       where
        nextTask = task { _active = False }
        updateTask currTask | _id currTask == _id nextTask = nextTask
                            | otherwise                    = currTask

  TaskMarkedAsDone _ id nextId -> state { _tasks = nextTasks }
   where
    tasks     = filter (not . _done) (_tasks state)
    taskFound = Task.findById id tasks
    nextTasks = case taskFound of
      Nothing   -> tasks
      Just task -> map updateTask tasks
       where
        nextTask = task { _id = nextId, _done = True }
        updateTask currTask | _id currTask == id = nextTask
                            | otherwise          = currTask

  TaskDeleted _ id -> state { _tasks = nextTasks }
    where nextTasks = filter ((/=) id . _id) (_tasks state)
