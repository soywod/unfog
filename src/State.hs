module State where

import Data.List
import Data.Maybe
import Data.Time
import Event
import Task

data State = State
  { getContext :: [Tag],
    getTasks :: [Task]
  }
  deriving (Show, Read)

rebuild :: [Event] -> State
rebuild = foldl apply (State [] [])

apply :: State -> Event -> State
apply state evt = case evt of
  TaskCreated _ id desc tags -> state {getTasks = tasks}
    where
      task = Task id desc tags False False [] []
      tasks = getTasks state ++ [task]
  TaskUpdated _ id desc tags -> state {getTasks = tasks}
    where
      tasks = map update (getTasks state)
      update task
        | id == (getId task) = task {getDesc = desc}
        | otherwise = task
  TaskStarted start id -> state {getTasks = tasks}
    where
      tasks = map update (getTasks state)
      update task
        | id == (getId task) = task {getActive = True, getStarts = getStarts task ++ [start]}
        | otherwise = task
  TaskStopped stop id -> state {getTasks = tasks}
    where
      tasks = map update (getTasks state)
      update task
        | id == (getId task) = task {getActive = False, getStops = getStops task ++ [stop]}
        | otherwise = task
  TaskMarkedAsDone _ id -> state {getTasks = tasks}
    where
      tasks = map update (getTasks state)
      update task
        | id == (getId task) = task {getDone = True}
        | otherwise = task
  TaskUnmarkedAsDone _ id -> state {getTasks = tasks}
    where
      tasks = map update (getTasks state)
      update task
        | id == (getId task) = task {getDone = False}
        | otherwise = task
  TaskDeleted _ id -> state {getTasks = tasks}
    where
      tasks = filter (not . (==) id . getId) (getTasks state)
  ContextUpdated _ ctx -> state {getContext = ctx}
