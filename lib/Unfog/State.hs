module Unfog.State where

import Control.Applicative ((<|>))
import Data.Maybe
import System.Directory (removeFile)
import Unfog.Event.Type (Event (..))
import qualified Unfog.File as File
import Unfog.Task hiding (new)

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

getCtx :: State -> Project
getCtx = _ctx

getTasks :: State -> [Task]
getTasks = _tasks

-- IO

fileName = ".state.cache"

readCache :: IO State
readCache = read <$> File.readFromName fileName

writeCache :: State -> IO ()
writeCache state = flip writeFile (show state) =<< File.getFullPath fileName

clearCache :: IO ()
clearCache = (removeFile =<< File.getFullPath fileName) <|> mempty

-- Event sourcing

rebuild :: [Event] -> State
rebuild = applyAll new

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
