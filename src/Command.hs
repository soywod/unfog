module Command where

import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Aeson              hiding ( Error )
import           Data.List                      ( union )
import           Data.Time
import           Control.Monad
import           Data.Time.Clock.POSIX

import           Event
import           Response
import           State
import           Store
import           Task
import qualified Parsec

data Command
  = CreateTask UTCTime Ref Id Pos Desc [Tag] (Maybe UTCTime)
  | UpdateTask UTCTime Ref Id Pos Desc [Tag] (Maybe UTCTime)
  | StartTask UTCTime Ref Id
  | StopTask UTCTime Ref Id
  | MarkAsDoneTask UTCTime Ref Id Id
  | DeleteTask UTCTime Ref Id
  | SetContext UTCTime [Tag]
  | Error String String
  | NoOp
  deriving (Show, Read)

type Subscriber = Parsec.ArgTree -> Command -> IO ()

subscribers :: [Subscriber]
subscribers = [logger]

handle :: Parsec.ArgTree -> IO ()
handle args = do
  now   <- getCurrentTime
  state <- applyEvents now <$> readEvents
  let cmds = getCommands now args state
  let evts = concatMap (execute state) cmds
  writeEvents evts
  mapM_ (forM_ cmds) $ map ($ args) subscribers

getCommands :: UTCTime -> Parsec.ArgTree -> State -> [Command]
getCommands t args state = case Parsec._cmd args of
  "create"  -> [createTask t args state]
  "update"  -> map (updateTask t args state) (Parsec._ids args)
  "replace" -> map (replaceTask t args state) (Parsec._ids args)
  "start"   -> map (startTask t args state) (Parsec._ids args)
  "stop"    -> map (stopTask t args state) (Parsec._ids args)
  "toggle"  -> map (toggleTask t args state) (Parsec._ids args)
  "done"    -> map (markAsDoneTask t args state) (Parsec._ids args)
  "delete"  -> map (deleteTask t args state) (Parsec._ids args)
  "remove"  -> map (removeTask t args state) (Parsec._ids args)
  "context" -> [setContext t args]
  _         -> [Error "" "invalid arguments"]

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
  due  = Parsec.parseDue t args

updateTask :: UTCTime -> Parsec.ArgTree -> State -> Id -> Command
updateTask t args state id = case maybeTask of
  Nothing   -> Error "update" $ "task [" ++ show id ++ "] not found"
  Just task -> validate task
 where
  tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
  maybeTask = findById id tasks
  newDesc   = Parsec._desc args
  nextDesc  = if null newDesc then maybe "" _desc maybeTask else newDesc
  newTags   = Parsec._tags args
  nextTags  = union newTags $ maybe [] _tags maybeTask
  validate task
    | _done task = Error "update" $ "task [" ++ show id ++ "] already done"
    | otherwise  = update task
  update task =
    let ref = _ref task
        id  = _id task
        pos = -1
        due = Nothing
    in  UpdateTask t ref id pos nextDesc nextTags due

replaceTask :: UTCTime -> Parsec.ArgTree -> State -> Int -> Command
replaceTask t args state id = case maybeTask of
  Nothing   -> Error "replace" $ "task [" ++ show id ++ "] not found"
  Just task -> validate task
 where
  tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
  maybeTask = findById id tasks
  nextDesc  = Parsec._desc args
  nextTags  = Parsec._tags args
  validate task
    | _done task = Error "replace" $ "task [" ++ show id ++ "] already done"
    | otherwise  = update task
  update task =
    let ref = _ref task
        id  = _id task
        pos = -1
        due = Nothing
    in  UpdateTask t ref id pos nextDesc nextTags due

startTask :: UTCTime -> Parsec.ArgTree -> State -> Id -> Command
startTask t args state id = case maybeTask of
  Nothing   -> Error "start" $ "task [" ++ show id ++ "] not found"
  Just task -> validate task
 where
  tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
  maybeTask = findById id tasks
  validate task
    | _done task = Error "start" $ "task [" ++ show id ++ "] already done"
    | _active task > 0 =  Error "start"
    $  "task ["
    ++ show id
    ++ "] already started"
    | otherwise = StartTask t (_ref task) (_id task)

stopTask :: UTCTime -> Parsec.ArgTree -> State -> Id -> Command
stopTask t args state id = case maybeTask of
  Nothing   -> Error "stop" $ "task [" ++ show id ++ "] not found"
  Just task -> validate task
 where
  tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
  maybeTask = findById id tasks
  validate task
    | _done task = Error "stop" $ "task [" ++ show id ++ "] already done"
    | _active task == 0 =  Error "stop"
    $  "task ["
    ++ show id
    ++ "] already stopped"
    | otherwise = StopTask t (_ref task) (_id task)

toggleTask :: UTCTime -> Parsec.ArgTree -> State -> Id -> Command
toggleTask t args state id = case maybeTask of
  Nothing   -> Error "toggle" $ "task [" ++ show id ++ "] not found"
  Just task -> validate task
 where
  tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
  maybeTask = findById id tasks
  validate task
    | _done task = Error "toggle" $ "task [" ++ show id ++ "] already done"
    | _active task > 0 = StopTask t (_ref task) (_id task)
    | otherwise = StartTask t (_ref task) (_id task)

markAsDoneTask :: UTCTime -> Parsec.ArgTree -> State -> Id -> Command
markAsDoneTask t args state id = case maybeTask of
  Nothing   -> Error "done" $ "task [" ++ show id ++ "] not found"
  Just task -> validate task
 where
  tasks      = filterByDone ("done" `elem` _ctx state) (_tasks state)
  maybeTask  = findById id tasks
  nextNumber = generateId $ filter _done $ _tasks state
  validate task
    | _done task = Error "done" $ "task [" ++ show id ++ "] already done"
    | otherwise  = MarkAsDoneTask t (_ref task) (_id task) nextNumber

deleteTask :: UTCTime -> Parsec.ArgTree -> State -> Id -> Command
deleteTask t args state id = case maybeTask of
  Nothing   -> Error "delete" $ "task [" ++ show id ++ "] not found"
  Just task -> DeleteTask t (_ref task) (_id task)
 where
  tasks     = filterByDone ("done" `elem` _ctx state) (_tasks state)
  maybeTask = findById id tasks

removeTask :: UTCTime -> Parsec.ArgTree -> State -> Id -> Command
removeTask t args state id = case maybeTask of
  Just task -> MarkAsDoneTask t (_ref task) (_id task) nextNumber
  Nothing   -> case maybeDoneTask of
    Just task -> DeleteTask t (_ref task) (_id task)
    Nothing   -> Error "remove" $ "task [" ++ show id ++ "] not found"
 where
  showDone      = "done" `elem` _ctx state
  tasks         = if showDone then [] else filterByDone False (_tasks state)
  doneTasks     = filterByDone True (_tasks state)
  nextNumber    = generateId doneTasks
  maybeTask     = findById id tasks
  maybeDoneTask = findById id doneTasks

setContext :: UTCTime -> Parsec.ArgTree -> Command
setContext t args = SetContext t (Parsec._tags args)

logger :: Subscriber
logger args cmd = case cmd of
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
