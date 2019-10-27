module Query where

import qualified Store
import           State

data Query = ShowTasks deriving (Show)

handle :: [String] -> IO ()
handle args = state >>= executeQuery
 where
  query        = parseArgs args
  state        = State.applyAll <$> Store.readAllEvents
  executeQuery = flip execute query

parseArgs :: [String] -> Query
parseArgs ("list" : args) = ShowTasks

execute :: State -> Query -> IO ()
execute state query = case query of
  ShowTasks -> print $ tasks state
