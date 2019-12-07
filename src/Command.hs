module Command where

import           Data.List
import           Data.Time
import           Text.Read
import           Data.Time.Clock.POSIX
import           Data.Aeson              hiding ( Error )
import qualified Data.ByteString.Lazy.Char8    as BL
import           Text.PrettyPrint.Boxes

import           Store
import           State
import           Task
import           Event
import           Utils
import           Response
import qualified Parsec

data Command
  = CreateTask UTCTime Ref Id Pos Desc [Tag] (Maybe Duration)
  | UpdateTask UTCTime Ref Id Pos Desc [Tag] (Maybe Duration)
  | StartTask UTCTime Ref Id
  | StopTask UTCTime Ref Id
  | MarkAsDoneTask UTCTime Ref Id Id
  | DeleteTask UTCTime Ref Id
  | SetContext UTCTime [Tag]
  | Error String String
  | NoOp
  deriving (Show, Read)

subscribers :: [Subscriber]
subscribers = [logger]

handle :: Parsec.ArgTree -> IO ()
handle args = do
  now   <- getCurrentTime
  state <- applyEvents now <$> readEvents
  let cmd  = getCommand now args state
  let evts = execute state cmd
  writeEvents evts
  notify args cmd subscribers

getCommand :: UTCTime -> Parsec.ArgTree -> State -> Command
getCommand t args state = case Parsec._cmd args of
  "create"  -> createTask t args state
  "update"  -> updateTask t args state
  "replace" -> replaceTask t args state
  "start"   -> startTask t args state
  "stop"    -> stopTask t args state
  "toggle"  -> toggleTask t args state
  "done"    -> markAsDoneTask t args state
  "delete"  -> deleteTask t args state
  "remove"  -> removeTask t args state
  "context" -> setContext t args
  _         -> Error "" "invalid arguments"

execute :: State -> Command -> [Event]
execute state command = case command of
  CreateTask t r i p desc tags due -> [TaskCreated t r i p desc tags due]
  UpdateTask t r i p desc tags due -> [TaskUpdated t r i p desc tags due]
  StartTask t ref id               -> [TaskStarted t ref id]
  StopTask  t ref id               -> [TaskStopped t ref id]
  MarkAsDoneTask t ref _ id        -> [TaskMarkedAsDone t ref id]
  DeleteTask t ref id              -> [TaskDeleted t ref id]
  SetContext t ctx                 -> [ContextSet t ctx]
  Error      _ _                   -> []
  NoOp                             -> []

createTask :: UTCTime -> Parsec.ArgTree -> State -> Command
createTask t args state = CreateTask t ref id pos desc tags due
 where
  ref  = floor $ 1000 * utcTimeToPOSIXSeconds t :: Int
  id   = generateId $ filter (not . _done) $ _tasks state
  pos  = -1
  desc = Parsec._desc args
  tags = Parsec._tags args `union` _ctx state
  due  = Nothing

updateTask :: UTCTime -> Parsec.ArgTree -> State -> Command
updateTask t args state = case Parsec._id args of
  0  -> Error "update" "invalid arguments"
  id -> case maybeTask of
    Nothing   -> Error "update" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
    maybeTask = findById id tasks
    newDesc   = Parsec._desc args
    nextDesc  = if null newDesc then maybe "" _desc maybeTask else newDesc
    newTags   = Parsec._tags args
    nextTags  = union newTags $ maybe [] _tags maybeTask
    validate task | _done task = Error "update" "task already done"
                  | otherwise  = update task
    update task =
      let ref = _ref task
          id  = _id task
          pos = -1
          due = Nothing
      in  UpdateTask t ref id pos nextDesc nextTags due

replaceTask :: UTCTime -> Parsec.ArgTree -> State -> Command
replaceTask t args state = case Parsec._id args of
  0  -> Error "replace" "invalid arguments"
  id -> case maybeTask of
    Nothing   -> Error "replace" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
    maybeTask = findById id tasks
    nextDesc  = Parsec._desc args
    nextTags  = Parsec._tags args
    validate task | _done task = Error "replace" "task already done"
                  | otherwise  = update task
    update task =
      let ref = _ref task
          id  = _id task
          pos = -1
          due = Nothing
      in  UpdateTask t ref id pos nextDesc nextTags due

startTask :: UTCTime -> Parsec.ArgTree -> State -> Command
startTask t args state = case Parsec._id args of
  0  -> Error "replace" "invalid arguments"
  id -> case maybeTask of
    Nothing   -> Error "start" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
    maybeTask = findById id tasks
    validate task | _done task       = Error "start" "task already done"
                  | _active task > 0 = Error "start" "task already started"
                  | otherwise        = StartTask t (_ref task) (_id task)

stopTask :: UTCTime -> Parsec.ArgTree -> State -> Command
stopTask t args state = case Parsec._id args of
  0  -> Error "replace" "invalid arguments"
  id -> case maybeTask of
    Nothing   -> Error "stop" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
    maybeTask = findById id tasks
    validate task | _done task        = Error "stop" "task already done"
                  | _active task == 0 = Error "stop" "task already stopped"
                  | otherwise         = StopTask t (_ref task) (_id task)

toggleTask :: UTCTime -> Parsec.ArgTree -> State -> Command
toggleTask t args state = case Parsec._id args of
  0  -> Error "replace" "invalid arguments"
  id -> case maybeTask of
    Nothing   -> Error "toggle" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
    maybeTask = findById id tasks
    validate task | _done task       = Error "toggle" "task already done"
                  | _active task > 0 = StopTask t (_ref task) (_id task)
                  | otherwise        = StartTask t (_ref task) (_id task)

markAsDoneTask :: UTCTime -> Parsec.ArgTree -> State -> Command
markAsDoneTask t args state = case Parsec._id args of
  0  -> Error "replace" "invalid arguments"
  id -> case maybeTask of
    Nothing   -> Error "done" "task not found"
    Just task -> validate task
   where
    tasks      = filterByDone ("done" `elem` _ctx state) (_tasks state)
    maybeTask  = findById id tasks
    nextNumber = generateId $ filter _done $ _tasks state
    validate task
      | _done task = Error "done" "task already done"
      | otherwise  = MarkAsDoneTask t (_ref task) (_id task) nextNumber

deleteTask :: UTCTime -> Parsec.ArgTree -> State -> Command
deleteTask t args state = case Parsec._id args of
  0  -> Error "replace" "invalid arguments"
  id -> case maybeTask of
    Nothing   -> Error "delete" "task not found"
    Just task -> DeleteTask t (_ref task) (_id task)
   where
    tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
    maybeTask = findById id tasks

removeTask :: UTCTime -> Parsec.ArgTree -> State -> Command
removeTask t args state = case Parsec._id args of
  0  -> Error "replace" "invalid arguments"
  id -> case maybeTask of
    Just task -> MarkAsDoneTask t (_ref task) (_id task) nextNumber
    Nothing   -> case maybeDoneTask of
      Just task -> DeleteTask t (_ref task) (_id task)
      Nothing   -> Error "remove" "task not found"
   where
    showDone      = "done" `elem` _ctx state
    tasks         = if showDone then [] else filterByDone False (_tasks state)
    doneTasks     = filterByDone True (_tasks state)
    nextNumber    = generateId doneTasks
    maybeTask     = findById id tasks
    maybeDoneTask = findById id doneTasks

setContext :: UTCTime -> Parsec.ArgTree -> Command
setContext t args = SetContext t (Parsec._tags args)

type Subscriber = Parsec.ArgTree -> Command -> IO ()
notify :: Parsec.ArgTree -> Command -> [Subscriber] -> IO ()
notify args command = foldr (\sub _ -> sub args command) (return ())

logger :: Subscriber
logger args event = case event of
  CreateTask _ _ id _ _ _ _ -> printAction id "created"
  UpdateTask _ _ id _ _ _ _ -> printAction id "updated"
  StartTask _ _ id          -> printAction id "started"
  StopTask  _ _ id          -> printAction id "stopped"
  MarkAsDoneTask _ _ id _   -> printAction id "done"
  DeleteTask _ _ id         -> printAction id "deleted"
  SetContext _ ctx          -> printCtx ctx
  Error cmd msg ->
    printErr rtype $ (if null cmd then "" else cmd ++ ": ") ++ msg
 where
  rtype = if Parsec._json (Parsec._opts args) then JSON else Text

  printAction id action =
    let msg = "task [" ++ show id ++ "] " ++ action in printMsg rtype msg

  printCtx ctx =
    let
      ctxStr = unwords ctx
      msg    = "context "
        ++ if null ctxStr then "cleared" else "[" ++ ctxStr ++ "] set"
    in
      printMsg rtype msg
