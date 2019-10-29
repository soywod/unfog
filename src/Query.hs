module Query where

import           Data.Maybe

import qualified Store
import           State
import           Task

data Query
  = NoOp
  | ShowTasks [String]
  | ShowTask Id [String]
  deriving (Show)

handle :: [String] -> IO ()
handle args = state >>= flip execute query
 where
  state = State.applyAll <$> Store.readAllEvents
  query = parseArgs args

parseArgs :: [String] -> Query
parseArgs ("list"      : args) = ShowTasks args
parseArgs ("show" : id : args) = ShowTask (read id) args

execute :: State -> Query -> IO ()
execute state query = case query of
  ShowTasks args -> print tasks
   where
    filterByDone = if "--done" `elem` args then _done else not . _done
    tasks        = filter filterByDone $ _tasks state

  ShowTask id args -> task >>= print
   where
    filterByDone = if "--done" `elem` args then _done else not . _done
    tasks        = filter filterByDone $ _tasks state
    foundTask    = findById id tasks
    task         = maybe (return ()) print foundTask
