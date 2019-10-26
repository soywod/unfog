module Query where

import           State
import           Store

data Query = ShowTasks deriving (Show)

handle :: [String] -> IO ()
handle args = do
  events <- readEvents
  let state = foldl State.apply State.new events
  let query = parseArgs args
  execute state query

parseArgs :: [String] -> Query
parseArgs ("list" : args) = ShowTasks

execute :: State -> Query -> IO ()
execute state query = case query of
  ShowTasks -> print $ tasks state
