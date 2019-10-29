module Task where

import           Control.Exception
import           Data.List                      ( find )

type Id = Int
type Desc = String
type Tag = String

data Task = Task { _id :: Id
                 , _desc :: Desc
                 , _tags :: [Tag]
                 , _active :: Bool
                 , _done :: Bool
                 } deriving (Show, Read)

emptyTask :: Task
emptyTask =
  Task { _id = 0, _desc = "", _tags = [], _active = False, _done = False }

findById :: Id -> [Task] -> Maybe Task
findById id = find $ (==) id . _id

filterByDone :: Bool -> [Task] -> [Task]
filterByDone predicate tasks = filteredTasks
 where
  byDone        = if predicate then _done else not . _done
  filteredTasks = filter byDone tasks
