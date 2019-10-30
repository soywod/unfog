module Query where

import           Control.Exception
import           Data.Maybe

import qualified Store
import           State
import           Task
import           Utils

data Query
  = ShowTasks [String]
  | ShowTask Id [String]
  | Log String
  deriving (Show)

handle :: [String] -> IO ()
handle args = state >>= flip execute query
 where
  state = State.applyAll <$> Store.readAll
  query = parseArgs args

parseArgs :: [String] -> Query
parseArgs ("list"      : args) = ShowTasks args
parseArgs ("show" : id : args) = case maybeRead id of
  Nothing -> Log "Id invalid"
  Just id -> ShowTask id args

execute :: State -> Query -> IO ()
execute state query = case query of
  ShowTasks args -> print tasks
   where
    showDone = "--done" `elem` args
    tasks    = Task.filterByDone showDone $ _tasks state

  ShowTask id args -> case maybeTask of
    Nothing   -> execute state $ Log "Task not found"
    Just task -> print task
   where
    showDone  = "--done" `elem` args
    tasks     = Task.filterByDone showDone $ _tasks state
    maybeTask = Task.findById id tasks

  Log message -> putStrLn message
