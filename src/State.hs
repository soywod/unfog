{-# LANGUAGE NamedFieldPuns #-}

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
      task = Task id desc tags
      tasks = getTasks state ++ [task]
  TaskUpdated _ id desc tags -> state
  TaskStarted start _ -> state
  TaskStopped stop _ -> state
  TaskMarkedAsDone stop id -> state
  TaskUnmarkedAsDone stop id -> state
  TaskDeleted _ _ -> state
  ContextUpdated _ ctx -> state

-- apply :: State -> Event -> State
-- apply state event = case event of
--   TaskCreated _ id pos desc tags -> state {getTasks = tasks}
--     where
--       task = emptyTask {id, pos, desc, tags}
--       tasks = getTasks state ++ [task]
--   TaskUpdated _ id pos desc tags -> state {getTasks = tasks}
--     where
--       update task = task {pos, desc, tags, due = due'}
--       tasks = getNextTasks ref update
--   TaskStarted start _ -> state {getTasks = getNextTasks ref update}
--     where
--       active = realToFrac $ diffUTCTime now start
--       update task = task {active = active, starts = starts task ++ [start]}
--   TaskStopped stop _ -> state {getTasks = getNextTasks ref update}
--     where
--       update task = task {active = 0, stops = stops task ++ [stop]}
--   TaskMarkedAsDone stop id -> state {getTasks = getNextTasks ref update}
--     where
--       update task =
--         let nextStops = stops task ++ [stop | active task > 0]
--          in task {id, active = 0, done = True, stops = nextStops}
--   TaskUnmarkedAsDone stop id -> state {getTasks = getNextTasks ref update}
--     where
--       update task = task {id, done = False}
--   TaskDeleted _ _ -> state {getTasks = nextTasks}
--     where
--       nextTasks = filter ((/=) ref . Task.ref) (tasks state)
--   ContextUpdated _ ctx -> state {getContext = ctx}
--   where
--     getNextTasks ref update =
--       case findByRef ref $ filterByTags (ctx state) (tasks state) of
--         Nothing -> tasks state
--         Just task ->
--           let nextTask = update task
--               save currTask
--                 | Task.ref currTask == Task.ref nextTask = nextTask
--                 | otherwise = currTask
--            in map save (tasks state)
