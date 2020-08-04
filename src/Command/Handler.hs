module Command.Handler where

import qualified Arg.Parser as Arg
import Data.List
import Data.Maybe
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

handle :: Arg.Command -> IO ()
handle arg = do
  now <- getCurrentTime
  state <- rebuild <$> readEvents
  nextId <- show <$> nextRandom
  let commands = parseCommands now state nextId arg
  let evts = concatMap (execute state) commands
  writeEvents evts

parseCommands :: UTCTime -> State -> Id -> Arg.Command -> [Command]
parseCommands now state id (Arg.Add desc) = [createTask now state id desc]
parseCommands now state _ (Arg.Edit id desc) = [updateTask now state id desc]
parseCommands now state _ (Arg.Start ids) = map (startTask now state) ids
parseCommands now state _ (Arg.Stop ids) = map (stopTask now state) ids
parseCommands now state _ (Arg.Do ids) = map (doTask now state) ids
parseCommands now state _ (Arg.Undo ids) = map (undoTask now state) ids
parseCommands now state _ (Arg.Delete ids) = map (deleteTask now state) ids

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

createTask :: UTCTime -> State -> Id -> Desc -> Command
createTask now state id desc = CreateTask now id desc (getContext state)

updateTask :: UTCTime -> State -> Id -> Desc -> Command
updateTask now state id desc = case maybeTask of
  Nothing -> Error "update" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | isNothing $ getDone task = Error "update" "task already done"
      | otherwise = UpdateTask now (getId task) desc (getTags task)

startTask :: UTCTime -> State -> Id -> Command
startTask now state id = case maybeTask of
  Nothing -> Error "start" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | isNothing $ getDone task = Error "start" "task already done"
      | not $ isNothing $ getActive task = Error "start" "task already started"
      | otherwise = StartTask now (getId task)

stopTask :: UTCTime -> State -> Id -> Command
stopTask now state id = case maybeTask of
  Nothing -> Error "stop" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | isNothing $ getDone task = Error "stop" "task already done"
      | isNothing $ getActive task = Error "stop" "task already stopped"
      | otherwise = StopTask now (getId task)

doTask :: UTCTime -> State -> Id -> Command
doTask now state id = case maybeTask of
  Nothing -> Error "done" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | isNothing $ getDone task = Error "done" "task already done"
      | otherwise = MarkAsDoneTask now (getId task)

undoTask :: UTCTime -> State -> Id -> Command
undoTask now state id = case maybeTask of
  Nothing -> Error "undone" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | not $ isNothing $ getDone task = Error "undone" "task not done"
      | otherwise = UnmarkAsDoneTask now (getId task)

deleteTask :: UTCTime -> State -> Id -> Command
deleteTask now state id = case findById id (getTasks state) of
  Nothing -> Error "delete" "task not found"
  Just task -> DeleteTask now (getId task)
