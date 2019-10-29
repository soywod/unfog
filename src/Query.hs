module Query where

import qualified Store
import           State
import           Task

data Query = ShowTasks [String] deriving (Show)

handle :: [String] -> IO ()
handle args = state >>= executeQuery
 where
  query        = parseArgs args
  state        = State.applyAll <$> Store.readAllEvents
  executeQuery = flip execute query

parseArgs :: [String] -> Query
parseArgs ("list" : args) = ShowTasks args

execute :: State -> Query -> IO ()
execute state query = case query of
  ShowTasks args -> print filteredTasks
   where
    tasks         = _tasks state
    filterByDone  = if "--done" `elem` args then _done else not . _done
    filteredTasks = filter filterByDone tasks
