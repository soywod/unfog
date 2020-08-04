module Command where

import Arg.Options
import qualified Arg.Parser as Arg
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
  | UpdateContext UTCTime ResponseType [Tag]
  | Error ResponseType String String
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

execute :: State -> Command -> [Event]
execute state cmd = case cmd of
  CreateTask now rtype id desc tags -> [TaskCreated now id desc tags]
  UpdateTask now rtype id desc tags -> [TaskUpdated now id desc tags]
  StartTask now rtype id -> [TaskStarted now id]
  StopTask now rtype id -> [TaskStopped now id]
  MarkAsDoneTask now rtype id -> [TaskMarkedAsDone now id]
  UnmarkAsDoneTask now rtype id -> [TaskUnmarkedAsDone now id]
  DeleteTask now rtype id -> [TaskDeleted now id]
  Error _ _ _ -> []

createTask :: UTCTime -> ResponseType -> State -> Id -> Desc -> Command
createTask now rtype state id desc = CreateTask now rtype id desc (getContext state)

updateTask :: UTCTime -> ResponseType -> State -> Id -> Desc -> Command
updateTask now rtype state id desc = case maybeTask of
  Nothing -> Error rtype "update" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | isNothing $ getDone task = Error rtype "update" "task already done"
      | otherwise = UpdateTask now rtype (getId task) desc (getTags task)

startTask :: UTCTime -> ResponseType -> State -> Id -> Command
startTask now rtype state id = case maybeTask of
  Nothing -> Error rtype "start" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | isNothing $ getDone task = Error rtype "start" "task already done"
      | not $ isNothing $ getActive task = Error rtype "start" "task already started"
      | otherwise = StartTask now rtype (getId task)

stopTask :: UTCTime -> ResponseType -> State -> Id -> Command
stopTask now rtype state id = case maybeTask of
  Nothing -> Error rtype "stop" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | isNothing $ getDone task = Error rtype "stop" "task already done"
      | isNothing $ getActive task = Error rtype "stop" "task already stopped"
      | otherwise = StopTask now rtype (getId task)

doTask :: UTCTime -> ResponseType -> State -> Id -> Command
doTask now rtype state id = case maybeTask of
  Nothing -> Error rtype "done" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | isNothing $ getDone task = Error rtype "done" "task already done"
      | otherwise = MarkAsDoneTask now rtype (getId task)

undoTask :: UTCTime -> ResponseType -> State -> Id -> Command
undoTask now rtype state id = case maybeTask of
  Nothing -> Error rtype "undone" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | not $ isNothing $ getDone task = Error rtype "undone" "task not done"
      | otherwise = UnmarkAsDoneTask now rtype (getId task)

deleteTask :: UTCTime -> ResponseType -> State -> Id -> Command
deleteTask now rtype state id = case findById id (getTasks state) of
  Nothing -> Error rtype "delete" "task not found"
  Just task -> DeleteTask now rtype (getId task)

-- Subscribers

type Subscriber = Command -> IO ()

subscribers :: [Subscriber]
subscribers = [logger]

notify :: [Command] -> [Subscriber] -> IO ()
notify cmds subs = mapM_ (notify' cmds) subs
  where
    notify' cmds subscriber = mapM_ subscriber cmds

logger :: Subscriber
logger cmd = case cmd of
  CreateTask _ rtype _ _ _ -> send rtype (MessageResponse "task created")
  UpdateTask _ rtype _ _ _ -> send rtype (MessageResponse "task updated")
  StartTask _ rtype _ -> send rtype (MessageResponse "task started")
  StopTask _ rtype _ -> send rtype (MessageResponse "task stopped")
  MarkAsDoneTask _ rtype _ -> send rtype (MessageResponse "task done")
  UnmarkAsDoneTask _ rtype _ -> send rtype (MessageResponse "task undone")
  DeleteTask _ rtype _ -> send rtype (MessageResponse "task deleted")
  UpdateContext _ rtype _ -> send rtype (MessageResponse "context updated")
  Error rtype cmd err -> send rtype (ErrorResponse cmd err)

-- where
--   rtype = if Parsec._json (Parsec._opts args) then JSON else Text
--   printAction id action =
--     let msg = "task [" ++ show id ++ "] " ++ action in printMsg rtype msg
--   printCtx ctx =
--     let ctxStr = unwords ctx
--         msg =
--           "context "
--             ++ if null ctxStr then "cleared" else "[" ++ ctxStr ++ "] set"
--      in printMsg rtype msg
