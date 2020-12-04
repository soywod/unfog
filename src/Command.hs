module Command where

import qualified ArgParser as Arg
import Control.Applicative ((<|>))
import Data.Maybe (isJust, isNothing)
import Data.Time (TimeZone, UTCTime, getCurrentTime, getCurrentTimeZone)
import qualified Data.UUID.V4 as UUID
import Event.Type (Event (..))
import Response
import State (State (..))
import qualified State
import qualified Store
import Task
import Text.Printf (printf)

data Command
  = AddTask UTCTime ResponseType Id Desc Project Due
  | EditTask UTCTime ResponseType Id Desc Project Due
  | StartTask UTCTime ResponseType Id
  | StopTask UTCTime ResponseType Id
  | DoTask UTCTime ResponseType Id
  | UndoTask UTCTime ResponseType Id
  | DeleteTask UTCTime ResponseType Id
  | UndeleteTask UTCTime ResponseType Id
  | EditContext UTCTime ResponseType Project
  | Error ResponseType String
  deriving (Show, Read, Eq)

handle :: Arg.Command -> IO ()
handle arg = do
  now <- getCurrentTime
  tzone <- getCurrentTimeZone
  state <- State.readCache <|> (State.rebuild <$> Store.readFile) <|> return State.new
  rndId <- show <$> UUID.nextRandom
  let cmds = parseCommands now tzone state rndId arg
  let evts = concatMap (execute state) cmds
  let state' = State.applyAll state evts
  let shortenId' = shortenId $ getShortIdLength $ State.getTasks state
  Store.appendFile evts
  State.writeCache state'
  notify shortenId' cmds subscribers

parseCommands :: UTCTime -> TimeZone -> State -> Id -> Arg.Command -> [Command]
parseCommands now tzone state id (Arg.AddTask desc proj due jsonOpt) = [addTask now tzone (parseResponseType jsonOpt) state id desc proj due]
parseCommands now tzone state _ (Arg.EditTask id desc proj due jsonOpt) = [editTask now tzone (parseResponseType jsonOpt) state id desc proj due]
parseCommands now _ state _ (Arg.StartTask ids jsonOpt) = map (startTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.StopTask ids jsonOpt) = map (stopTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.ToggleTask ids jsonOpt) = map (toggleTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.DoTask ids jsonOpt) = map (doTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.UndoTask ids jsonOpt) = map (undoTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.DeleteTask ids jsonOpt) = map (deleteTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ state _ (Arg.UndeleteTask ids jsonOpt) = map (undeleteTask now (parseResponseType jsonOpt) state) ids
parseCommands now _ _ _ (Arg.EditContext ctx jsonOpt) = [editContext now (parseResponseType jsonOpt) ctx]

execute :: State -> Command -> [Event]
execute _ cmd = case cmd of
  AddTask now _ id desc proj due -> [TaskAdded now id desc proj due]
  EditTask now _ id desc proj due -> [TaskEdited now id desc proj due]
  StartTask now _ id -> [TaskStarted now id]
  StopTask now _ id -> [TaskStopped now id]
  DoTask now _ id -> [TaskDid now id]
  UndoTask now _ id -> [TaskUndid now id]
  DeleteTask now _ id -> [TaskDeleted now id]
  UndeleteTask now _ id -> [TaskUndeleted now id]
  EditContext now _ ctx -> [ContextEdited now ctx]
  Error _ _ -> []

addTask :: UTCTime -> TimeZone -> ResponseType -> State -> Id -> Desc -> Project -> Due -> Command
addTask now _ rtype (State ctx _) id desc proj due
  | null desc = Error rtype "desc missing"
  | otherwise = AddTask now rtype id desc proj' due
  where
    proj' = if isNothing proj then ctx else proj

editTask :: UTCTime -> TimeZone -> ResponseType -> State -> Id -> Desc -> Project -> Due -> Command
editTask now _ rtype (State _ tasks) id desc proj due = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | otherwise = EditTask now rtype (getId task) desc' proj' due
      where
        desc' = if null desc then getDesc task else desc
        proj' = if isNothing proj then getProject task else proj

-- due' = if isNothing due then getDue task else due

startTask :: UTCTime -> ResponseType -> State -> Id -> Command
startTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | isJust $ getActive task = Error rtype "task already started"
      | otherwise = StartTask now rtype (getId task)

stopTask :: UTCTime -> ResponseType -> State -> Id -> Command
stopTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | isNothing $ getActive task = Error rtype "task already stopped"
      | otherwise = StopTask now rtype (getId task)

toggleTask :: UTCTime -> ResponseType -> State -> Id -> Command
toggleTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | isJust $ getActive task = StopTask now rtype $ getId task
      | otherwise = StartTask now rtype $ getId task

doTask :: UTCTime -> ResponseType -> State -> Id -> Command
doTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isJust $ getDone task = Error rtype "task already done"
      | otherwise = DoTask now rtype (getId task)

undoTask :: UTCTime -> ResponseType -> State -> Id -> Command
undoTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | isNothing $ getDone task = Error rtype "task not yet done"
      | otherwise = UndoTask now rtype (getId task)

deleteTask :: UTCTime -> ResponseType -> State -> Id -> Command
deleteTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Error rtype "task already deleted"
      | otherwise = DeleteTask now rtype (getId task)

undeleteTask :: UTCTime -> ResponseType -> State -> Id -> Command
undeleteTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isNothing $ getDeleted task = Error rtype "task not yet deleted"
      | otherwise = UndeleteTask now rtype (getId task)

editContext :: UTCTime -> ResponseType -> Project -> Command
editContext = EditContext

-- Subscribers

type ShortenId = Id -> ShortId

type Subscriber = ShortenId -> Command -> IO ()

notify :: ShortenId -> [Command] -> [Subscriber] -> IO ()
notify shortenId cmds = mapM_ (notify' cmds)
  where
    notify' cmds subscriber = mapM_ (subscriber shortenId) cmds

subscribers :: [Subscriber]
subscribers = [logger]

logger :: Subscriber
logger shortenId cmd = case cmd of
  AddTask _ Text id _ proj _ -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m added%s" (shortenId id) $ showIfJust " to project \x1b[34m%s\x1b[0m" proj
  AddTask _ Json id _ proj _ -> send Json $ MessageResponse $ printf "Task %s added%s" (shortenId id) $ showIfJust " to project %s" proj
  EditTask _ Text id _ _ _ -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m edited" (shortenId id)
  EditTask _ Json id _ _ _ -> send Json $ MessageResponse $ printf "Task %s edited" (shortenId id)
  StartTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m started" (shortenId id)
  StartTask _ Json id -> send Json $ MessageResponse $ printf "Task %s started" (shortenId id)
  StopTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m stopped" (shortenId id)
  StopTask _ Json id -> send Json $ MessageResponse $ printf "Task %s stopped" (shortenId id)
  DoTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m done" (shortenId id)
  DoTask _ Json id -> send Json $ MessageResponse $ printf "Task %s done" (shortenId id)
  UndoTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m undone" (shortenId id)
  UndoTask _ Json id -> send Json $ MessageResponse $ printf "Task %s undone" (shortenId id)
  DeleteTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m deleted" (shortenId id)
  DeleteTask _ Json id -> send Json $ MessageResponse $ printf "Task %s deleted" (shortenId id)
  UndeleteTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m undeleted" (shortenId id)
  UndeleteTask _ Json id -> send Json $ MessageResponse $ printf "Task %s undeleted" (shortenId id)
  EditContext _ rtype Nothing -> send rtype $ MessageResponse "Context cleared"
  EditContext _ Text (Just ctx) -> send Text $ MessageResponse $ printf "Context set [\x1b[34m%s\x1b[0m]" ctx
  EditContext _ Json (Just ctx) -> send Json $ MessageResponse $ printf "Context set [%s]" ctx
  Error rtype msg -> send rtype $ ErrorResponse msg
  where
    showIfJust _ Nothing = ""
    showIfJust msg (Just a) = printf msg a
