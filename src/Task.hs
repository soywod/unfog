module Task where

import Data.Aeson
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Time
import Data.UUID hiding (null)
import Duration
import Table

type Id = String

type Pos = Int

type Desc = String

type Tag = String

type Active = Maybe UTCTime

type Due = Maybe UTCTime

type Done = Maybe UTCTime

type Deleted = Maybe UTCTime

data Task = Task
  { getId :: Id,
    getDesc :: Desc,
    getTags :: [Tag],
    getStarts :: [UTCTime],
    getStops :: [UTCTime],
    getDue :: Due,
    getActive :: Active,
    getDone :: Done,
    getDeleted :: Deleted
  }
  deriving (Show, Read, Eq)

findById :: Id -> [Task] -> Maybe Task
findById id = find $ isPrefixOf id . getId

findFstActiveTask :: [Task] -> Maybe Task
findFstActiveTask tasks
  | null activeTasks = Nothing
  | otherwise = Just (head activeTasks)
  where
    activeTasks = filter (not . isNothing . getActive) tasks
