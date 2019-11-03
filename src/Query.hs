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
  ShowTasks args -> do
    putStrLn contextLog
    prettyPrint tasks
   where
    fByTags    = filterByTags $ _context state
    fByDone    = filterByDone $ _showDone state
    tasks      = fByTags . fByDone $ _tasks state
    contextStr = unwords $ [ "done" | _showDone state ] ++ _context state
    contextLog = "unfog: list"
      ++ if null contextStr then "" else " [" ++ contextStr ++ "]"

  ShowTask id args -> showTaskIfExists
   where
    fByTags          = filterByTags $ _context state
    fByDone          = filterByDone $ _showDone state
    maybeTask        = findById id $ fByTags . fByDone $ _tasks state
    showTask         = prettyPrint . flip (:) [] <$> maybeTask
    showError        = elog "show" "task not found"
    showTaskIfExists = fromMaybe showError showTask

  ShowWorktime args -> do
    now <- getCurrentTime
    let tags      = filter startsByPlus args
    let ids       = map _id $ filterByTags args $ _tasks state
    let worktimes = foldl (getWorktime now ids) [] events
    let durations = map (mapToDuration now) worktimes
    let total = sum $ map snd durations
    putStrLn $ "unfog: worktime " ++ if null args
      then "global"
      else "[" ++ unwords tags ++ "]"
    putStrLn $ approximativeDuration total

  Error command message -> elog command message

mapToDuration :: UTCTime -> Worktime -> (Id, Micro)
mapToDuration now (id, (starts, stops)) = (id, diff)
 where
  lastStop = [ now | length starts > length stops ]
  diff     = realToFrac $ sum $ zipWith diffUTCTime (stops ++ lastStop) starts

type Worktime = (Id, ([UTCTime], [UTCTime]))
getWorktime :: UTCTime -> [Id] -> [Worktime] -> Event -> [Worktime]
getWorktime now ids acc event = case event of
  TaskStarted start id _ -> ifMatchId id $ case lookupAcc id of
    Nothing -> (id, ([start], [])) : acc
    Just (_, (starts, stops)) ->
      (id, (starts ++ [start], stops)) : accWithout id

  TaskStopped stop id _ -> ifMatchId id $ case lookupAcc id of
    Nothing -> (id, ([], [stop])) : acc
    Just (_, (starts, stops)) ->
      (id, (starts, stops ++ [stop])) : accWithout id

  TaskDeleted _ id _ -> accWithout id

  _                  -> acc
 where
  ifMatchId id next = if id `elem` ids then next else acc
  lookupAcc id = find ((==) id . fst) acc
  accWithout id = filter ((/=) id . fst) acc
