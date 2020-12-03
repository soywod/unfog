module State where

import Data.Maybe
import Event.Type (Event (..))
import qualified File
import Task hiding (new)

-- Model

data State = State
  { _ctx :: Project,
    _tasks :: [Task]
  }
  deriving (Show, Read, Eq)

new :: State
new =
  State
    { _ctx = Nothing,
      _tasks = []
    }

-- Getters

getContext :: State -> Project
getContext = _ctx

getTasks :: State -> [Task]
getTasks = _tasks

-- Read & Write

readFile :: IO State
readFile = read <$> File.readFromName "state"

writeFile :: State -> IO ()
writeFile state = writeFile' state' =<< File.getPath "state"
  where
    state' = show state
    writeFile' = flip Prelude.writeFile

-- Event sourcing

rebuild :: [Event] -> State
rebuild = applyAll $ State Nothing []

applyAll :: State -> [Event] -> State
applyAll = foldl apply

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
        | id == getId task = task {_active = Nothing, _done = Just now, _stops = getStops task ++ [now | isJust $ getActive task]}
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
        | id == getId task = task {_active = Nothing, _deleted = Just now, _stops = getStops task ++ [now | isJust $ getActive task]}
        | otherwise = task
  TaskUndeleted _ id -> state {_tasks = tasks}
    where
      tasks = map update $ getTasks state
      update task
        | id == getId task = task {_deleted = Nothing}
        | otherwise = task
  ContextEdited _ ctx -> state {_ctx = ctx}
