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
  = AddTask UTCTime ResponseType Id Desc Project Due
  | EditTask UTCTime ResponseType Id Desc Project Due
  | StartTask UTCTime ResponseType Id
  | StopTask UTCTime ResponseType Id
  | DoTask UTCTime ResponseType Id
  | UndoTask UTCTime ResponseType Id
  | DeleteTask UTCTime ResponseType Id
  | UndeleteTask UTCTime ResponseType Id
  | EditContext ResponseType Project
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
parseCommands now tzone state id (Arg.Add desc proj due jsonOpt) = [addTask now tzone (parseResponseType jsonOpt) state id desc proj due]
parseCommands now tzone state _ (Arg.Edit id desc proj due jsonOpt) = [editTask now tzone (parseResponseType jsonOpt) state id desc proj due]
parseCommands now _ state _ (Arg.Start ids jsonOpt) = map (startTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Stop ids jsonOpt) = map (stopTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Do ids jsonOpt) = map (doTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Undo ids jsonOpt) = map (undoTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Delete ids jsonOpt) = map (deleteTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Undelete ids jsonOpt) = map (undeleteTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.Context ctx jsonOpt) = [editContext (parseResponseType jsonOpt) ctx]

execute :: State -> Command -> [Event]
execute state cmd = case cmd of
  AddTask now rtype id desc proj due -> [TaskAdded now id desc proj due]
  EditTask now rtype id desc proj due -> [TaskEdited now id desc proj due]
  StartTask now rtype id -> [TaskStarted now id]
  StopTask now rtype id -> [TaskStopped now id]
  DoTask now rtype id -> [TaskDid now id]
  UndoTask now rtype id -> [TaskUndid now id]
  DeleteTask now rtype id -> [TaskDeleted now id]
  UndeleteTask now rtype id -> [TaskUndeleted now id]
  EditContext rtype ctx -> [ContextEdited ctx]
  Error _ _ -> []

addTask :: UTCTime -> TimeZone -> ResponseType -> State -> Id -> Desc -> Project -> Due -> Command
addTask now tzone rtype (State ctx tasks) id desc proj due
  | null desc = Error rtype "desc missing"
  | otherwise = AddTask now rtype id desc proj' due
  where
    proj' = if isNothing proj then ctx else proj

editTask :: UTCTime -> TimeZone -> ResponseType -> State -> Id -> Desc -> Project -> Due -> Command
editTask now tzone rtype (State ctx tasks) id desc proj due = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | otherwise = EditTask now rtype (getId task) desc proj' due
      where
        proj' = if isNothing proj then ctx else proj

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
      | otherwise = DoTask now rtype (getId task)

undoTask :: UTCTime -> ResponseType -> State -> Id -> Command
undoTask now rtype state id = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isNothing $ getDone task = Error rtype "task not yet done"
      | otherwise = UndoTask now rtype (getId task)

deleteTask :: UTCTime -> ResponseType -> State -> Id -> Command
deleteTask now rtype state id = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | otherwise = DeleteTask now rtype (getId task)

undeleteTask :: UTCTime -> ResponseType -> State -> Id -> Command
undeleteTask now rtype state id = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isNothing $ getDeleted task = Error rtype "task not yet deleted"
      | otherwise = UndeleteTask now rtype (getId task)

editContext :: ResponseType -> Project -> Command
editContext = EditContext

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
  AddTask _ rtype _ _ _ _ -> send rtype $ CommandResponse "task" "created"
  EditTask _ rtype _ _ _ _ -> send rtype $ CommandResponse "task" "updated"
  StartTask _ rtype _ -> send rtype $ CommandResponse "task" "started"
  StopTask _ rtype _ -> send rtype $ CommandResponse "task" "stopped"
  DoTask _ rtype _ -> send rtype $ CommandResponse "task" "done"
  UndoTask _ rtype _ -> send rtype $ CommandResponse "task" "undone"
  DeleteTask _ rtype _ -> send rtype $ CommandResponse "task" "deleted"
  UndeleteTask _ rtype _ -> send rtype $ CommandResponse "task" "undeleted"
  EditContext Text ctx -> do
    send Text $ ContextResponse ctx
    putStrLn ""
    Query.handle $ Arg.List False False False
  Error rtype msg -> send rtype $ ErrorResponse msg
