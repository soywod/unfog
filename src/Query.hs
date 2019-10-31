module Query where

import           Control.Exception
import           Data.Maybe
import           Text.Read
import           Data.Time.Clock
import           Data.Duration
import           Data.Fixed
import           Data.List
import           Text.PrettyPrint.Boxes

import qualified Store
import           State
import           Task
import           Utils
import           Event

data Query
  = ShowTasks [String]
  | ShowTask Id [String]
  | ShowWorktime [String]
  | Error String String
  deriving (Show)

handle :: [String] -> IO ()
handle args = do
  events <- Store.readAll
  let state = State.applyAll events
  let query = parseArgs args
  execute state events query

parseArgs :: [String] -> Query
parseArgs args = case args of
  ("list"      : args) -> ShowTasks args
  ("worktime"  : args) -> ShowWorktime args
  ("show" : id : args) -> case readMaybe id of
    Nothing -> Error "show" "task not found"
    Just id -> ShowTask id args

execute :: State -> [Event] -> Query -> IO ()
execute state events query = case query of
  ShowTasks args -> prettyPrint tasks
   where
    shouldShowDone = "--done" `elem` args
    tasks          = filterByDone shouldShowDone $ _tasks state

  ShowWorktime args -> showWorktime state events args

  ShowTask id args  -> case maybeTask of
    Nothing   -> elog "show" "task not found"
    Just task -> prettyPrint [task]
   where
    shouldShowDone = "--done" `elem` args
    tasks          = filterByDone shouldShowDone $ _tasks state
    maybeTask      = findById id tasks

  Error command message -> elog command message

showWorktime state events args = do
  now <- getCurrentTime
  let (ids, tags) = getFilters ([], []) args
  let allIds      = ids `union` getIdsByTags tags
  let worktimes   = map (getWorktime now events) allIds
  let total       = sum $ map snd worktimes
  putStrLn
    $  render
    $  table
    $  [["ID", "WORKTIME"]]
    ++ map formatWorktime worktimes
    ++ [["TOTAL", approximativeDuration total]]
 where
  getIdsByTags tags = map _id $ filter
    (not . null . intersect tags . map ("+" ++) . _tags)
    (_tasks state)
  formatWorktime (id, worktime) = [show id, approximativeDuration worktime]

getWorktime :: UTCTime -> [Event] -> Id -> (Id, Micro)
getWorktime now events currId =
  (currId, realToFrac $ sum $ zipWith diffUTCTime (stops ++ lastStop) starts)
 where
  starts   = map byDate $ filter onlyStarted events
  stops    = map byDate $ filter onlyStopped events
  lastStop = [ now | length starts > length stops ]
  onlyStarted event = case event of
    TaskStarted _ id -> id == currId
    _                -> False
  onlyStopped event = case event of
    TaskStopped _ id -> id == currId
    _                -> False
  byDate event = case event of
    TaskStarted start _ -> start
    TaskStopped stop  _ -> stop

getFilters :: ([Id], [Tag]) -> [String] -> ([Id], [Tag])
getFilters filters     []           = filters
getFilters (ids, tags) (arg : args) = case readMaybe arg :: Maybe Int of
  Nothing -> getFilters (ids, tags ++ [arg]) args
  Just id -> getFilters (ids ++ [id], tags) args
