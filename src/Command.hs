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

data Command
  = CreateTask UTCTime Reference Id Position Description [Tag]
  | UpdateTask UTCTime Reference Id Position Description [Tag]
  | StartTask UTCTime Reference Id
  | StopTask UTCTime Reference Id
  | MarkAsDoneTask UTCTime Reference Id Id
  | DeleteTask UTCTime Reference Id
  | SetContext UTCTime Bool [String]
  | Error String String
  | NoOp
  deriving (Show, Read)

handle :: ResponseType -> [String] -> IO ()
handle rtype args = do
  now   <- getCurrentTime
  state <- State.applyAll <$> Store.readAll
  let command     = parseArgs now state args
  let events      = execute state command
  let subscribers = [logger]
  write events
  notify rtype command subscribers

execute :: State -> Command -> [Event]
execute state command = case command of
  CreateTask t ref id pos desc tags -> [TaskCreated t ref id pos desc tags]
  UpdateTask t ref id pos desc tags -> [TaskUpdated t ref id pos desc tags]
  StartTask t ref id                -> [TaskStarted t ref id]
  StopTask  t ref id                -> [TaskStopped t ref id]
  MarkAsDoneTask t ref _ id         -> [TaskMarkedAsDone t ref id]
  DeleteTask t ref      id          -> [TaskDeleted t ref id]
  SetContext t showDone context     -> [ContextSet t showDone context]
  Error _ _                         -> []
  NoOp                              -> []

parseArgs :: UTCTime -> State -> [String] -> Command
parseArgs t state args = case args of
  ("create"  : args) -> createTask t state args
  ("update"  : args) -> updateTask t state args
  ("replace" : args) -> replaceTask t state args
  ("start"   : args) -> startTask t state args
  ("stop"    : args) -> stopTask t state args
  ("toggle"  : args) -> toggleTask t state args
  ("done"    : args) -> markAsDoneTask t state args
  ("delete"  : args) -> deleteTask t state args
  ("remove"  : args) -> removeTask t state args
  ("context" : args) -> setContext t args
  (command   : _   ) -> Error command "command not found"
  []                 -> Error "" "command missing"

createTask t state args = case args of
  []   -> Error "create" "missing desc"
  args -> CreateTask t ref id (-1) desc tags
   where
    ref  = floor $ 1000 * utcTimeToPOSIXSeconds t :: Int
    id   = generateId $ filter (not . _done) $ _tasks state
    desc = unwords $ filter (not . startsByPlus) args
    tags = filter startsByPlus args `union` _context state

updateTask t state args = case args of
  []          -> Error "update" "missing id"
  [_        ] -> Error "update" "missing args"
  (id : args) -> case maybeTask of
    Nothing   -> Error "update" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe id >>= flip findById tasks
    newDesc   = unwords $ filter (not . startsByPlus) args
    nextDesc  = if newDesc == "" then maybe "" _desc maybeTask else newDesc
    newTags   = filter startsByPlus args
    nextTags  = union newTags $ maybe [] _tags maybeTask
    validate task
      | _done task = Error "update" "task already done"
      | otherwise  = UpdateTask t (_ref task) (_id task) (-1) nextDesc nextTags

replaceTask t state args = case args of
  []          -> Error "replace" "missing id"
  [_        ] -> Error "replace" "missing args"
  (id : args) -> case maybeTask of
    Nothing   -> Error "replace" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe id >>= flip findById tasks
    nextDesc  = unwords $ filter (not . startsByPlus) args
    nextTags  = filter startsByPlus args
    validate task
      | _done task = Error "replace" "task already done"
      | otherwise  = UpdateTask t (_ref task) (_id task) (-1) nextDesc nextTags

startTask t state args = case args of
  []       -> Error "start" "missing id"
  (id : _) -> case maybeTask of
    Nothing   -> Error "start" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe id >>= flip findById tasks
    validate task | _done task   = Error "start" "task already done"
                  | _active task = Error "start" "task already started"
                  | otherwise    = StartTask t (_ref task) (_id task)

stopTask t state args = case args of
  []       -> Error "stop" "missing id"
  (id : _) -> case maybeTask of
    Nothing   -> Error "stop" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe id >>= flip findById tasks
    validate task | _done task           = Error "stop" "task already done"
                  | (not . _active) task = Error "stop" "task already stopped"
                  | otherwise            = StopTask t (_ref task) (_id task)

toggleTask t state args = case args of
  []       -> Error "toggle" "missing id"
  (id : _) -> case maybeTask of
    Nothing   -> Error "toggle" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe id >>= flip findById tasks
    validate task | _done task   = Error "toggle" "task already done"
                  | _active task = StopTask t (_ref task) (_id task)
                  | otherwise    = StartTask t (_ref task) (_id task)

markAsDoneTask t state args = case args of
  []       -> Error "done" "missing id"
  (id : _) -> case maybeTask of
    Nothing   -> Error "done" "task not found"
    Just task -> validate task
   where
    tasks      = filterByDone (_showDone state) (_tasks state)
    maybeTask  = readMaybe id >>= flip findById tasks
    nextNumber = generateId $ filter _done $ _tasks state
    validate task
      | _done task = Error "done" "task already done"
      | otherwise  = MarkAsDoneTask t (_ref task) (_id task) nextNumber

deleteTask t state args = case args of
  []       -> Error "delete" "missing id"
  (id : _) -> case maybeTask of
    Nothing   -> Error "delete" "task not found"
    Just task -> DeleteTask t (_ref task) (_id task)
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe id >>= flip findById tasks

removeTask t state args = case args of
  []       -> Error "remove" "missing id"
  (id : _) -> case maybeTask of
    Just task -> MarkAsDoneTask t (_ref task) (_id task) nextNumber
    Nothing   -> case maybeDoneTask of
      Just task -> DeleteTask t (_ref task) (_id task)
      Nothing   -> Error "remove" "task not found"
   where
    tasks = if _showDone state then [] else filterByDone False (_tasks state)
    doneTasks     = filterByDone True (_tasks state)
    nextNumber    = generateId doneTasks
    maybeTask     = readMaybe id >>= flip findById tasks
    maybeDoneTask = readMaybe id >>= flip findById doneTasks

setContext t args = SetContext t showDone ctx
 where
  showDone = "done" `elem` args
  ctx      = filter startsByPlus args

type Subscriber = ResponseType -> Command -> IO ()
notify :: ResponseType -> Command -> [Subscriber] -> IO ()
notify rtype command = foldr (\sub _ -> sub rtype command) (return ())

logger :: Subscriber
logger rtype event = case event of
  CreateTask _ _ n _ _ _    -> printAction rtype n "created"
  UpdateTask _ _ n _ _ _    -> printAction rtype n "updated"
  StartTask _ _ n           -> printAction rtype n "started"
  StopTask  _ _ n           -> printAction rtype n "stopped"
  MarkAsDoneTask _ _ n _    -> printAction rtype n "done"
  DeleteTask _ _        n   -> printAction rtype n "deleted"
  SetContext _ showDone ctx -> printCtx rtype showDone ctx
  Error cmd msg             -> printErr rtype $ cmd ++ ": " ++ msg
 where
  printAction rtype n action =
    let msg = "task [" ++ show n ++ "] " ++ action in printMsg rtype msg

  printCtx rtype showDone ctx =
    let
      ctxStr = unwords ([ "done" | showDone ] ++ ctx)
      msg    = "context "
        ++ if null ctxStr then "cleared" else "[" ++ ctxStr ++ "] set"
    in
      printMsg rtype msg
