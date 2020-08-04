module Command where

import ArgOptions
import qualified ArgParser as Arg
import Data.List
import Data.Maybe
import Data.Time
import Data.UUID.V4
import Event
import Response
import State
import Task
import Prelude hiding (id)

data Command
  = CreateTask UTCTime ResponseType Id Desc [Tag]
  | UpdateTask UTCTime ResponseType Id Desc [Tag]
  | StartTask UTCTime ResponseType Id
  | StopTask UTCTime ResponseType Id
  | MarkAsDoneTask UTCTime ResponseType Id
  | UnmarkAsDoneTask UTCTime ResponseType Id
  | DeleteTask UTCTime ResponseType Id
  | UpdateContext ResponseType Context
  | Error ResponseType String
  deriving (Show, Read)

handle :: Arg.Command -> IO ()
handle arg = do
  now <- getCurrentTime
  state <- rebuild <$> readEvents
  nextId <- show <$> nextRandom
  let cmds = parseCommands now state nextId arg
  let evts = concatMap (execute state) cmds
  writeEvents evts
  notify cmds subscribers

parseCommands :: UTCTime -> State -> Id -> Arg.Command -> [Command]
parseCommands now state id (Arg.Add desc jsonOpt) = [createTask now (parseResponseType jsonOpt) state id desc]
parseCommands now state _ (Arg.Edit id desc jsonOpt) = [updateTask now (parseResponseType jsonOpt) state id desc]
parseCommands now state _ (Arg.Start ids jsonOpt) = map (startTask now (parseResponseType jsonOpt) state) ids
parseCommands now state _ (Arg.Stop ids jsonOpt) = map (stopTask now (parseResponseType jsonOpt) state) ids
parseCommands now state _ (Arg.Do ids jsonOpt) = map (doTask now (parseResponseType jsonOpt) state) ids
parseCommands now state _ (Arg.Undo ids jsonOpt) = map (undoTask now (parseResponseType jsonOpt) state) ids
parseCommands now state _ (Arg.Delete ids jsonOpt) = map (deleteTask now (parseResponseType jsonOpt) state) ids
parseCommands now state _ (Arg.Context ctx jsonOpt) = [updateContext (parseResponseType jsonOpt) ctx]

execute :: State -> Command -> [Event]
execute state cmd = case cmd of
  CreateTask now rtype id desc tags -> [TaskCreated now id desc tags]
  UpdateTask now rtype id desc tags -> [TaskUpdated now id desc tags]
  StartTask now rtype id -> [TaskStarted now id]
  StopTask now rtype id -> [TaskStopped now id]
  MarkAsDoneTask now rtype id -> [TaskMarkedAsDone now id]
  UnmarkAsDoneTask now rtype id -> [TaskUnmarkedAsDone now id]
  DeleteTask now rtype id -> [TaskDeleted now id]
  UpdateContext rtype ctx -> [ContextUpdated ctx]
  Error _ _ -> []

createTask :: UTCTime -> ResponseType -> State -> Id -> Desc -> Command
createTask now rtype state id desc = CreateTask now rtype id desc (getContext state)

updateTask :: UTCTime -> ResponseType -> State -> Id -> Desc -> Command
updateTask now rtype state id desc = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | otherwise = UpdateTask now rtype (getId task) desc (getTags task)

startTask :: UTCTime -> ResponseType -> State -> Id -> Command
startTask now rtype state id = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | isJust $ getActive task = Error rtype "task already started"
      | otherwise = StartTask now rtype (getId task)

stopTask :: UTCTime -> ResponseType -> State -> Id -> Command
stopTask now rtype state id = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | isNothing $ getActive task = Error rtype "task already stopped"
      | otherwise = StopTask now rtype (getId task)

doTask :: UTCTime -> ResponseType -> State -> Id -> Command
doTask now rtype state id = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | otherwise = MarkAsDoneTask now rtype (getId task)

undoTask :: UTCTime -> ResponseType -> State -> Id -> Command
undoTask now rtype state id = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isNothing $ getDone task = Error rtype "task not done"
      | otherwise = UnmarkAsDoneTask now rtype (getId task)

deleteTask :: UTCTime -> ResponseType -> State -> Id -> Command
deleteTask now rtype state id = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | otherwise = DeleteTask now rtype (getId task)

updateContext :: ResponseType -> Context -> Command
updateContext rtype ctx = UpdateContext rtype ctx

-- Subscribers

type Subscriber = Command -> IO ()

notify :: [Command] -> [Subscriber] -> IO ()
notify cmds subs = mapM_ (notify' cmds) subs
  where
    notify' cmds subscriber = mapM_ subscriber cmds

subscribers :: [Subscriber]
subscribers = [logger]

logger :: Subscriber
logger cmd = case cmd of
  CreateTask _ rtype _ _ _ -> send rtype $ CommandResponse "task" "created"
  UpdateTask _ rtype _ _ _ -> send rtype $ CommandResponse "task" "updated"
  StartTask _ rtype _ -> send rtype $ CommandResponse "task" "started"
  StopTask _ rtype _ -> send rtype $ CommandResponse "task" "stopped"
  MarkAsDoneTask _ rtype _ -> send rtype $ CommandResponse "task" "done"
  UnmarkAsDoneTask _ rtype _ -> send rtype $ CommandResponse "task" "undone"
  DeleteTask _ rtype _ -> send rtype $ CommandResponse "task" "deleted"
  UpdateContext rtype ctx -> send rtype $ ContextResponse ctx
  Error rtype msg -> send rtype $ ErrorResponse msg
