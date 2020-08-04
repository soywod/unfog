module State (State (..), Context, rebuild, apply, getVisibleTasks) where

import Data.List
import Data.Maybe
import Data.Time
import Event
import Task

type Context = [Tag]

data State = State
  { getContext :: Context,
    getTasks :: [Task]
  }
  deriving (Show, Read)

rebuild :: [Event] -> State
rebuild = foldl apply (State [] [])

apply :: State -> Event -> State
apply state evt = case evt of
  TaskCreated _ id desc tags -> state {getTasks = tasks}
    where
      task = Task id desc tags [] [] Nothing Nothing Nothing
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
        | id == (getId task) = task {getActive = Just start, getStarts = getStarts task ++ [start]}
        | otherwise = task
  TaskStopped stop id -> state {getTasks = tasks}
    where
      tasks = map update (getTasks state)
      update task
        | id == (getId task) = task {getActive = Nothing, getStops = getStops task ++ [stop]}
        | otherwise = task
  TaskMarkedAsDone now id -> state {getTasks = tasks}
    where
      tasks = map update (getTasks state)
      update task
        | id == (getId task) = task {getDone = Just now}
        | otherwise = task
  TaskUnmarkedAsDone _ id -> state {getTasks = tasks}
    where
      tasks = map update (getTasks state)
      update task
        | id == (getId task) = task {getDone = Nothing}
        | otherwise = task
  TaskDeleted now id -> state {getTasks = tasks}
    where
      tasks = map update (getTasks state)
      update task
        | id == (getId task) = task {getDeleted = Just now}
        | otherwise = task
  ContextUpdated ctx -> state {getContext = ctx}

getVisibleTasks :: State -> [Task]
getVisibleTasks state = filter notDone $ filter notDeleted $ filter (matchContext ctx) $ tasks
  where
    ctx = getContext state
    tasks = getTasks state

matchContext :: Context -> Task -> Bool
matchContext ctx task
  | null ctx = True
  | otherwise = not $ null $ intersect ctx $ getTags task

notDone :: Task -> Bool
notDone = isNothing . getDone

notDeleted :: Task -> Bool
notDeleted = isNothing . getDeleted
