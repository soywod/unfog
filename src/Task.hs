module Task where

import Data.List
import Data.Maybe
import Data.Time

-- Model

type Id = String

type Desc = String

type Project = Maybe String

type Starts = [UTCTime]

type Stops = [UTCTime]

type Due = Maybe UTCTime

type Active = Maybe UTCTime

type Done = Maybe UTCTime

type Deleted = Maybe UTCTime

data Task = Task
  { _id :: Id,
    _desc :: Desc,
    _project :: Project,
    _starts :: [UTCTime],
    _stops :: [UTCTime],
    _due :: Due,
    _active :: Active,
    _done :: Done,
    _deleted :: Deleted
  }
  deriving (Show, Read, Eq)

-- Getters

getId :: Task -> Id
getId = _id

getDesc :: Task -> Desc
getDesc = _desc

getProject :: Task -> Project
getProject = _project

getStarts :: Task -> Starts
getStarts = _starts

getStops :: Task -> Stops
getStops = _stops

getDue :: Task -> Due
getDue = _due

getActive :: Task -> Active
getActive = _active

getDone :: Task -> Done
getDone = _done

getDeleted :: Task -> Deleted
getDeleted = _deleted

-- Predicates

type Predicate = Task -> Bool

notDone :: Predicate
notDone = isNothing . getDone

notDeleted :: Predicate
notDeleted = isNothing . getDeleted

matchContext :: Project -> Predicate
matchContext project task
  | isNothing project = True
  | otherwise = project == getProject task

isDuePassed :: UTCTime -> Task -> Bool
isDuePassed now task = case getDue task of
  Nothing -> False
  Just due -> due < now

filterWith :: [Predicate] -> [Task] -> [Task]
filterWith predicates = filter matchAll
  where
    matchAll task = foldl1 (&&) $ map ($ task) predicates

-- Finders

findById :: Id -> [Task] -> Maybe Task
findById id = find $ isPrefixOf id . getId

findFstActive :: [Task] -> Maybe Task
findFstActive tasks
  | null activeTasks = Nothing
  | otherwise = Just $ head activeTasks
  where
    activeTasks = filter (not . isNothing . getActive) tasks
