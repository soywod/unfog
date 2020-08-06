module Command where

import ArgOptions
import qualified ArgParser as Arg
import Data.List
import Data.Maybe
import Data.Time
import Data.UUID.V4
import Event
import qualified Query
import Response
import State
import Task
import Prelude hiding (id)

data Command
  = CreateTask UTCTime ResponseType Id Desc [Tag] Due
  | UpdateTask UTCTime ResponseType Id Desc [Tag] Due
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
  tzone <- getCurrentTimeZone
  state <- rebuild <$> readEvents
  nextId <- show <$> nextRandom
  let cmds = parseCommands now tzone state nextId arg
  let evts = concatMap (execute state) cmds
  writeEvents evts
  notify cmds subscribers

parseCommands :: UTCTime -> TimeZone -> State -> Id -> Arg.Command -> [Command]
parseCommands now tzone state id (Arg.Add args jsonOpt) = [createTask now tzone (parseResponseType jsonOpt) state id args]
parseCommands now tzone state _ (Arg.Edit id args jsonOpt) = [updateTask now tzone (parseResponseType jsonOpt) state id args]
parseCommands now tzone state _ (Arg.Set id args jsonOpt) = [replaceTask now tzone (parseResponseType jsonOpt) state id args]
parseCommands now _ state _ (Arg.Start ids jsonOpt) = map (startTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Stop ids jsonOpt) = map (stopTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Do ids jsonOpt) = map (doTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Undo ids jsonOpt) = map (undoTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Delete ids jsonOpt) = map (deleteTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Context ctx jsonOpt) = [updateContext (parseResponseType jsonOpt) ctx]

execute :: State -> Command -> [Event]
execute state cmd = case cmd of
  CreateTask now rtype id desc tags due -> [TaskCreated now id desc tags due]
  UpdateTask now rtype id desc tags due -> [TaskUpdated now id desc tags due]
  StartTask now rtype id -> [TaskStarted now id]
  StopTask now rtype id -> [TaskStopped now id]
  MarkAsDoneTask now rtype id -> [TaskMarkedAsDone now id]
  UnmarkAsDoneTask now rtype id -> [TaskUnmarkedAsDone now id]
  DeleteTask now rtype id -> [TaskDeleted now id]
  UpdateContext rtype ctx -> [ContextUpdated ctx]
  Error _ _ -> []

createTask :: UTCTime -> TimeZone -> ResponseType -> State -> Id -> Arg.CustomArgs -> Command
createTask now tzone rtype state id (desc, tags, due)
  | null desc = Error rtype "desc missing"
  | otherwise = CreateTask now rtype id desc' tags' due'
  where
    desc' = unwords desc
    tags' = getContext state `union` tags
    due' = Arg.parseDate now tzone 0 0 due

updateTask :: UTCTime -> TimeZone -> ResponseType -> State -> Id -> Arg.CustomArgs -> Command
updateTask now tzone rtype state id (desc, tags, due) = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | otherwise = UpdateTask now rtype (getId task) desc' tags' due'
      where
        desc' = if null desc then (getDesc task) else unwords desc
        tags' = getContext state `union` getTags task `union` tags
        due' = Arg.parseDate now tzone 0 0 due

replaceTask :: UTCTime -> TimeZone -> ResponseType -> State -> Id -> Arg.CustomArgs -> Command
replaceTask now tzone rtype state id (desc, tags, due) = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | otherwise = UpdateTask now rtype (getId task) desc' tags' due'
      where
        desc' = unwords desc
        tags' = getContext state `union` tags
        due' = Arg.parseDate now tzone 0 0 due

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
  CreateTask _ rtype _ _ _ _ -> send rtype $ CommandResponse "task" "created"
  UpdateTask _ rtype _ _ _ _ -> send rtype $ CommandResponse "task" "updated"
  StartTask _ rtype _ -> send rtype $ CommandResponse "task" "started"
  StopTask _ rtype _ -> send rtype $ CommandResponse "task" "stopped"
  MarkAsDoneTask _ rtype _ -> send rtype $ CommandResponse "task" "done"
  UnmarkAsDoneTask _ rtype _ -> send rtype $ CommandResponse "task" "undone"
  DeleteTask _ rtype _ -> send rtype $ CommandResponse "task" "deleted"
  UpdateContext Json ctx -> send Json $ ContextResponse ctx
  UpdateContext Text ctx -> do
    send Text $ ContextResponse ctx
    putStrLn ""
    Query.handle $ Arg.List False False False
  Error rtype msg -> send rtype $ ErrorResponse msg
