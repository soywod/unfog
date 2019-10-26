module Query where

import           State
import           Store

data Query = ShowTasks deriving (Show)

handle :: [String] -> IO ()
handle args = state >>= flip execute query
 where
  query = parseArgs args
  state = apply <$> readEvents

parseArgs :: [String] -> Query
parseArgs ("list" : args) = ShowTasks

execute :: State -> Query -> IO ()
execute state query = case query of
  ShowTasks -> print $ tasks state
