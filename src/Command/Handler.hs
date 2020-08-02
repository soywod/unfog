module Command.Handler where

import qualified Arg.Parser as Arg
import Data.List
import Data.Time
import Data.UUID.V4
import Event
import State
import Task
import Prelude hiding (id)

data Command
  = CreateTask UTCTime Id Desc [Tag]
  | UpdateTask UTCTime Id Desc [Tag]
  | StartTask UTCTime Id
  | StopTask UTCTime Id
  | MarkAsDoneTask UTCTime Id
  | UnmarkAsDoneTask UTCTime Id
  | DeleteTask UTCTime Id
  | UpdateContext UTCTime [Tag]
  | Error String String
  deriving (Show, Read)

handleCommand :: Arg.Command -> IO ()
handleCommand cmd = do
  now <- getCurrentTime
  state <- rebuild <$> readEvents
  case cmd of
    Arg.Add desc -> do
      id <- show <$> nextRandom
      writeEvents $ execute state $ createTask state now id desc
    Arg.Edit id desc -> writeEvents $ execute state $ updateTask state now id desc
    Arg.Start ids -> writeEvents $ concatMap (execute state) $ map (startTask state now) ids
    Arg.Stop ids -> writeEvents $ concatMap (execute state) $ map (stopTask state now) ids
    Arg.Do ids -> writeEvents $ concatMap (execute state) $ map (doTask state now) ids
    Arg.Undo ids -> writeEvents $ concatMap (execute state) $ map (undoTask state now) ids
    Arg.Delete ids -> writeEvents $ concatMap (execute state) $ map (deleteTask state now) ids

execute :: State -> Command -> [Event]
execute state cmd = case cmd of
  CreateTask now id desc tags -> [TaskCreated now id desc tags]
  UpdateTask now id desc tags -> [TaskUpdated now id desc tags]
  StartTask now id -> [TaskStarted now id]
  StopTask now id -> [TaskStopped now id]
  MarkAsDoneTask now id -> [TaskMarkedAsDone now id]
  UnmarkAsDoneTask now id -> [TaskUnmarkedAsDone now id]
  DeleteTask now id -> [TaskDeleted now id]
  Error _ _ -> []

createTask :: State -> UTCTime -> Id -> Desc -> Command
createTask state now id desc = CreateTask now id desc (getContext state)

updateTask :: State -> UTCTime -> Id -> Desc -> Command
updateTask state now id desc = case maybeTask of
  Nothing -> Error "update" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | getDone task = Error "update" "task already done"
      | otherwise = UpdateTask now (getId task) desc (getTags task)

startTask :: State -> UTCTime -> Id -> Command
startTask state now id = case maybeTask of
  Nothing -> Error "start" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | getDone task = Error "start" "task already done"
      | getActive task = Error "start" "task already started"
      | otherwise = StartTask now (getId task)

stopTask :: State -> UTCTime -> Id -> Command
stopTask state now id = case maybeTask of
  Nothing -> Error "stop" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | getDone task = Error "stop" "task already done"
      | (not . getActive) task = Error "stop" "task already stopped"
      | otherwise = StopTask now (getId task)

doTask :: State -> UTCTime -> Id -> Command
doTask state now id = case maybeTask of
  Nothing -> Error "done" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | getDone task = Error "done" "task already done"
      | otherwise = MarkAsDoneTask now (getId task)

undoTask :: State -> UTCTime -> Id -> Command
undoTask state now id = case maybeTask of
  Nothing -> Error "undone" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | (not . getDone) task = Error "undone" "task not done"
      | otherwise = UnmarkAsDoneTask now (getId task)

deleteTask :: State -> UTCTime -> Id -> Command
deleteTask state now id = case findById id (getTasks state) of
  Nothing -> Error "delete" "task not found"
  Just task -> DeleteTask now (getId task)
