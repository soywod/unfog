module State (State (..), getContext, getTasks, rebuild, apply) where

import Data.List
import Data.Maybe
import Data.Time
import Event
import Task

data State = State
  { _ctx :: Project,
    _tasks :: [Task]
  }
  deriving (Show, Read)

getContext :: State -> Project
getContext = _ctx

getTasks :: State -> [Task]
getTasks = _tasks

rebuild :: [Event] -> State
rebuild = foldl apply $ State Nothing []

apply :: State -> Event -> State
apply state evt = case evt of
  TaskAdded _ id desc project due -> state {_tasks = tasks}
    where
      task = Task id desc project [] [] due Nothing Nothing Nothing
      tasks = getTasks state ++ [task]
  TaskEdited _ id desc project due -> state {_tasks = tasks}
    where
      tasks = map update $ getTasks state
      update task
        | id == getId task = task {_desc = desc, _project = project, _due = due}
        | otherwise = task
  TaskStarted start id -> state {_tasks = tasks}
    where
      tasks = map update $ getTasks state
      update task
        | id == getId task = task {_active = Just start, _starts = getStarts task ++ [start]}
        | otherwise = task
  TaskStopped stop id -> state {_tasks = tasks}
    where
      tasks = map update $ getTasks state
      update task
        | id == getId task = task {_active = Nothing, _stops = getStops task ++ [stop]}
        | otherwise = task
  TaskDid now id -> state {_tasks = tasks}
    where
      tasks = map update $ getTasks state
      update task
        | id == getId task = task {_done = Just now}
        | otherwise = task
  TaskUndid _ id -> state {_tasks = tasks}
    where
      tasks = map update $ getTasks state
      update task
        | id == getId task = task {_done = Nothing}
        | otherwise = task
  TaskDeleted now id -> state {_tasks = tasks}
    where
      tasks = map update $ getTasks state
      update task
        | id == getId task = task {_deleted = Just now}
        | otherwise = task
  TaskUndeleted now id -> state {_tasks = tasks}
    where
      tasks = map update $ getTasks state
      update task
        | id == getId task = task {_deleted = Nothing}
        | otherwise = task
  ContextEdited ctx -> state {_ctx = ctx}
