module Task where

import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Time
import qualified Data.Set as Set

-- Model

type Id = String

type ShortId = String

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

new :: Task
new =
  Task
    { _id = "",
      _desc = "",
      _project = Nothing,
      _starts = [],
      _stops = [],
      _due = Nothing,
      _active = Nothing,
      _done = Nothing,
      _deleted = Nothing
    }

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

both :: Predicate -> Predicate -> Predicate
both f g x = f x && g x

filterWith :: [Predicate] -> [Task] -> [Task]
filterWith predicates = filter matchAll
  where
    matchAll = foldl1 both predicates

-- Finders

findById :: Id -> [Task] -> Maybe Task
findById id = find $ isPrefixOf id' . getId
  where
    id' = filter (/= '-') id

findFstActive :: [Task] -> Maybe Task
findFstActive tasks
  | null activeTasks = Nothing
  | otherwise = Just $ head activeTasks
  where
    activeTasks = filter (not . isNothing . getActive) tasks

shortenId :: Int -> String -> ShortId
shortenId len id = take len $ filter (/= '-') id

getShortIdLength :: [Task] -> Int
getShortIdLength tasks = loop 4
  where
    ids = map getId tasks
    loop len =
      if len >= 32
        then 32
        else
          if Set.size distinctIds == length tasks
            then len
            else loop (len + 1)
      where
        distinctIds = Set.fromList $ map (take len) ids
