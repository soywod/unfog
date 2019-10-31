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
  | Error String String
  deriving (Show)

handle :: [String] -> IO ()
handle args = state >>= flip execute query
 where
  state = State.applyAll <$> Store.readAll
  query = parseArgs args

parseArgs :: [String] -> Query
parseArgs ("list"      : args) = ShowTasks args
parseArgs ("show" : id : args) = case maybeRead id of
  Nothing -> Error "show" "task not found"
  Just id -> ShowTask id args

execute :: State -> Query -> IO ()
execute state query = case query of
  ShowTasks args -> print tasks
   where
    shouldShowDone = "--done" `elem` args
    tasks          = filterByDone shouldShowDone $ _tasks state

  ShowTask id args -> case maybeTask of
    Nothing   -> execute state $ Error "show" "task not found"
    Just task -> print task
   where
    shouldShowDone = "--done" `elem` args
    tasks          = filterByDone shouldShowDone $ _tasks state
    maybeTask      = findById id tasks

  Error command message -> elog command message
