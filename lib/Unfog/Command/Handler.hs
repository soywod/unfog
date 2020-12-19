module Unfog.Command.Handler where

import Control.Applicative ((<|>))
import Data.Maybe (isJust, isNothing)
import Data.Time (TimeZone, UTCTime, getCurrentTime, getCurrentTimeZone)
import qualified Data.UUID.V4 as UUID
import Text.Printf (printf)
import qualified Unfog.Arg.Types as Arg
import Unfog.Command.Types (Command, ShortenId, Subscriber)
import qualified Unfog.Command.Types as Command
import Unfog.Event.Type (Event (..))
import Unfog.Response
import Unfog.State (State (..))
import qualified Unfog.State as State
import qualified Unfog.Store as Store
import Unfog.Task

addTask :: UTCTime -> TimeZone -> ResponseType -> State -> Id -> Desc -> Project -> Due -> Command
addTask now _ rtype (State ctx _) id desc proj due
  | null desc = Command.Error rtype "desc missing"
  | otherwise = Command.AddTask now rtype id desc proj' due
  where
    proj' = if isNothing proj then ctx else proj

editTask :: UTCTime -> TimeZone -> ResponseType -> State -> Id -> Desc -> Project -> Due -> Command
editTask now _ rtype (State _ tasks) id desc proj due = case findById id tasks of
  Nothing -> Command.Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Command.Error rtype "task already deleted"
      | isJust $ getDone task = Command.Error rtype "task already done"
      | otherwise = Command.EditTask now rtype (getId task) desc' proj' due
      where
        desc' = if null desc then getDesc task else desc
        proj' = if isNothing proj then getProject task else proj

startTask :: UTCTime -> ResponseType -> State -> Id -> Command
startTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Command.Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Command.Error rtype "task already deleted"
      | isJust $ getDone task = Command.Error rtype "task already done"
      | isJust $ getActive task = Command.Error rtype "task already started"
      | otherwise = Command.StartTask now rtype (getId task)

stopTask :: UTCTime -> ResponseType -> State -> Id -> Command
stopTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Command.Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Command.Error rtype "task already deleted"
      | isJust $ getDone task = Command.Error rtype "task already done"
      | isNothing $ getActive task = Command.Error rtype "task already stopped"
      | otherwise = Command.StopTask now rtype (getId task)

toggleTask :: UTCTime -> ResponseType -> State -> Id -> Command
toggleTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Command.Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Command.Error rtype "task already deleted"
      | isJust $ getDone task = Command.Error rtype "task already done"
      | isJust $ getActive task = Command.StopTask now rtype $ getId task
      | otherwise = Command.StartTask now rtype $ getId task

doTask :: UTCTime -> ResponseType -> State -> Id -> Command
doTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Command.Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Command.Error rtype "task already deleted"
      | isJust $ getDone task = Command.Error rtype "task already done"
      | otherwise = Command.DoTask now rtype (getId task)

undoTask :: UTCTime -> ResponseType -> State -> Id -> Command
undoTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Command.Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Command.Error rtype "task already deleted"
      | isNothing $ getDone task = Command.Error rtype "task not yet done"
      | otherwise = Command.UndoTask now rtype (getId task)

deleteTask :: UTCTime -> ResponseType -> State -> Id -> Command
deleteTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Command.Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isJust $ getDeleted task = Command.Error rtype "task already deleted"
      | otherwise = Command.DeleteTask now rtype (getId task)

undeleteTask :: UTCTime -> ResponseType -> State -> Id -> Command
undeleteTask now rtype (State _ tasks) id = case findById id tasks of
  Nothing -> Command.Error rtype "task not found"
  Just task -> validate task
  where
    validate task
      | isNothing $ getDeleted task = Command.Error rtype "task not yet deleted"
      | otherwise = Command.UndeleteTask now rtype (getId task)

editContext :: UTCTime -> ResponseType -> Project -> Command
editContext = Command.EditContext

notify :: ShortenId -> [Command] -> [Subscriber] -> IO ()
notify shortenId cmds = mapM_ (notify' cmds)
  where
    notify' cmds subscriber = mapM_ (subscriber shortenId) cmds

subscribers :: [Subscriber]
subscribers = [logger]

logger :: Subscriber
logger shortenId cmd = case cmd of
  Command.AddTask _ Text id _ proj _ -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m added%s" (shortenId id) $ showIfJust " to project \x1b[34m%s\x1b[0m" proj
  Command.AddTask _ Json id _ proj _ -> send Json $ MessageResponse $ printf "Task %s added%s" (shortenId id) $ showIfJust " to project %s" proj
  Command.EditTask _ Text id _ _ _ -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m edited" (shortenId id)
  Command.EditTask _ Json id _ _ _ -> send Json $ MessageResponse $ printf "Task %s edited" (shortenId id)
  Command.StartTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m started" (shortenId id)
  Command.StartTask _ Json id -> send Json $ MessageResponse $ printf "Task %s started" (shortenId id)
  Command.StopTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m stopped" (shortenId id)
  Command.StopTask _ Json id -> send Json $ MessageResponse $ printf "Task %s stopped" (shortenId id)
  Command.DoTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m done" (shortenId id)
  Command.DoTask _ Json id -> send Json $ MessageResponse $ printf "Task %s done" (shortenId id)
  Command.UndoTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m undone" (shortenId id)
  Command.UndoTask _ Json id -> send Json $ MessageResponse $ printf "Task %s undone" (shortenId id)
  Command.DeleteTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m deleted" (shortenId id)
  Command.DeleteTask _ Json id -> send Json $ MessageResponse $ printf "Task %s deleted" (shortenId id)
  Command.UndeleteTask _ Text id -> send Text $ MessageResponse $ printf "Task \x1b[31m%s\x1b[0m undeleted" (shortenId id)
  Command.UndeleteTask _ Json id -> send Json $ MessageResponse $ printf "Task %s undeleted" (shortenId id)
  Command.EditContext _ rtype Nothing -> send rtype $ MessageResponse "Context cleared"
  Command.EditContext _ Text (Just ctx) -> send Text $ MessageResponse $ printf "Context set [\x1b[34m%s\x1b[0m]" ctx
  Command.EditContext _ Json (Just ctx) -> send Json $ MessageResponse $ printf "Context set [%s]" ctx
  Command.Error rtype msg -> send rtype $ ErrorResponse msg
  where
    showIfJust _ Nothing = ""
    showIfJust msg (Just a) = printf msg a

commandsOfArg :: UTCTime -> TimeZone -> State -> Id -> Arg.Command -> [Command]
commandsOfArg now tzone state id (Arg.AddTask desc proj due jsonOpt) = [addTask now tzone (parseResponseType jsonOpt) state id desc proj due]
commandsOfArg now tzone state _ (Arg.EditTask id desc proj due jsonOpt) = [editTask now tzone (parseResponseType jsonOpt) state id desc proj due]
commandsOfArg now _ state _ (Arg.StartTask ids jsonOpt) = map (startTask now (parseResponseType jsonOpt) state) ids
commandsOfArg now _ state _ (Arg.StopTask ids jsonOpt) = map (stopTask now (parseResponseType jsonOpt) state) ids
commandsOfArg now _ state _ (Arg.ToggleTask ids jsonOpt) = map (toggleTask now (parseResponseType jsonOpt) state) ids
commandsOfArg now _ state _ (Arg.DoTask ids jsonOpt) = map (doTask now (parseResponseType jsonOpt) state) ids
commandsOfArg now _ state _ (Arg.UndoTask ids jsonOpt) = map (undoTask now (parseResponseType jsonOpt) state) ids
commandsOfArg now _ state _ (Arg.DeleteTask ids jsonOpt) = map (deleteTask now (parseResponseType jsonOpt) state) ids
commandsOfArg now _ state _ (Arg.UndeleteTask ids jsonOpt) = map (undeleteTask now (parseResponseType jsonOpt) state) ids
commandsOfArg now _ _ _ (Arg.EditContext ctx jsonOpt) = [editContext now (parseResponseType jsonOpt) ctx]

execute :: State -> Command -> [Event]
execute _ cmd = case cmd of
  Command.AddTask now _ id desc proj due -> [TaskAdded now id desc proj due]
  Command.EditTask now _ id desc proj due -> [TaskEdited now id desc proj due]
  Command.StartTask now _ id -> [TaskStarted now id]
  Command.StopTask now _ id -> [TaskStopped now id]
  Command.DoTask now _ id -> [TaskDid now id]
  Command.UndoTask now _ id -> [TaskUndid now id]
  Command.DeleteTask now _ id -> [TaskDeleted now id]
  Command.UndeleteTask now _ id -> [TaskUndeleted now id]
  Command.EditContext now _ ctx -> [ContextEdited now ctx]
  Command.Error _ _ -> []

handle :: Arg.Command -> IO ()
handle arg = do
  now <- getCurrentTime
  tzone <- getCurrentTimeZone
  state <- State.readCache <|> (State.rebuild <$> Store.readFile) <|> return State.new
  rndId <- show <$> UUID.nextRandom
  let cmds = commandsOfArg now tzone state rndId arg
  let evts = concatMap (execute state) cmds
  let state' = State.applyAll state evts
  let shortenId' = shortenId $ getShortIdLength $ State.getTasks state
  Store.appendFile evts
  State.writeCache state'
  notify shortenId' cmds subscribers
