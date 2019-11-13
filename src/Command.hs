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
  = CreateTask UTCTime Id Number Desc [Tag]
  | UpdateTask UTCTime Id Number Desc [Tag]
  | ReplaceTask UTCTime Id Number Desc [Tag]
  | StartTask UTCTime Id Number
  | StopTask UTCTime Id Number
  | ToggleTask UTCTime Id Number
  | MarkAsDoneTask UTCTime Id Number Number
  | DeleteTask UTCTime Id Number
  | RemoveTask UTCTime Id Number
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
  CreateTask time id number desc tags -> [TaskCreated time id number desc tags]
  UpdateTask time id number desc tags -> [TaskUpdated time id number desc tags]
  ReplaceTask time id number desc tags ->
    [TaskReplaced time id number desc tags]
  StartTask time id number         -> [TaskStarted time id number]
  StopTask  time id number         -> [TaskStopped time id number]
  MarkAsDoneTask time id _ number  -> [TaskMarkedAsDone time id number]
  DeleteTask time id       number  -> [TaskDeleted time id number]
  SetContext time showDone context -> [ContextSet time showDone context]
  Error _ _                        -> []
  NoOp                             -> []

parseArgs :: UTCTime -> State -> [String] -> Command
parseArgs time state args = case args of
  ("create"  : args) -> createTask time state args
  ("update"  : args) -> updateTask time state args
  ("replace" : args) -> replaceTask time state args
  ("start"   : args) -> startTask time state args
  ("stop"    : args) -> stopTask time state args
  ("toggle"  : args) -> toggleTask time state args
  ("done"    : args) -> markAsDoneTask time state args
  ("delete"  : args) -> deleteTask time state args
  ("remove"  : args) -> removeTask time state args
  ("context" : args) -> setContext time args
  (command   : _   ) -> Error command "command not found"
  []                 -> Error "" "command missing"

createTask time state args = case args of
  []   -> Error "create" "missing desc"
  args -> CreateTask time id number desc tags
   where
    id     = floor $ 1000 * utcTimeToPOSIXSeconds time :: Int
    number = generateNumber $ filter (not . _done) $ _tasks state
    desc   = unwords $ filter (not . startsByPlus) args
    tags   = filter startsByPlus args `union` _context state

updateTask time state args = case args of
  []              -> Error "update" "missing id"
  [_            ] -> Error "update" "missing args"
  (number : args) -> case maybeTask of
    Nothing   -> Error "update" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks
    newDesc   = unwords $ filter (not . startsByPlus) args
    nextDesc  = if newDesc == "" then maybe "" _desc maybeTask else newDesc
    newTags   = filter startsByPlus args
    nextTags  = union newTags $ maybe [] _tags maybeTask
    validate task
      | _done task = Error "update" "task already done"
      | otherwise  = UpdateTask time (_id task) (_number task) nextDesc nextTags

replaceTask time state args = case args of
  []              -> Error "replace" "missing id"
  [_            ] -> Error "replace" "missing args"
  (number : args) -> case maybeTask of
    Nothing   -> Error "replace" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks
    nextDesc  = unwords $ filter (not . startsByPlus) args
    nextTags  = filter startsByPlus args
    validate task
      | _done task = Error "replace" "task already done"
      | otherwise = ReplaceTask time (_id task) (_number task) nextDesc nextTags

startTask time state args = case args of
  []           -> Error "start" "missing id"
  (number : _) -> case maybeTask of
    Nothing   -> Error "start" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks
    validate task | _done task   = Error "start" "task already done"
                  | _active task = Error "start" "task already started"
                  | otherwise    = StartTask time (_id task) (_number task)

stopTask time state args = case args of
  []           -> Error "stop" "missing id"
  (number : _) -> case maybeTask of
    Nothing   -> Error "stop" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks
    validate task | _done task = Error "stop" "task already done"
                  | (not . _active) task = Error "stop" "task already stopped"
                  | otherwise = StopTask time (_id task) (_number task)

toggleTask time state args = case args of
  []           -> Error "toggle" "missing id"
  (number : _) -> case maybeTask of
    Nothing   -> Error "toggle" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks
    validate task | _done task   = Error "toggle" "task already done"
                  | _active task = StopTask time (_id task) (_number task)
                  | otherwise    = StartTask time (_id task) (_number task)

markAsDoneTask time state args = case args of
  []           -> Error "done" "missing id"
  (number : _) -> case maybeTask of
    Nothing   -> Error "done" "task not found"
    Just task -> validate task
   where
    tasks      = filterByDone (_showDone state) (_tasks state)
    maybeTask  = readMaybe number >>= flip findByNumber tasks
    nextNumber = generateNumber $ filter _done $ _tasks state
    validate task
      | _done task = Error "done" "task already done"
      | otherwise  = MarkAsDoneTask time (_id task) (_number task) nextNumber

deleteTask time state args = case args of
  []           -> Error "delete" "missing id"
  (number : _) -> case maybeTask of
    Nothing   -> Error "delete" "task not found"
    Just task -> DeleteTask time (_id task) (_number task)
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks

removeTask time state args = case args of
  []           -> Error "remove" "missing id"
  (number : _) -> case maybeTask of
    Just task -> MarkAsDoneTask time (_id task) (_number task) nextNumber
    Nothing   -> case maybeDoneTask of
      Just task -> DeleteTask time (_id task) (_number task)
      Nothing   -> Error "remove" "task not found"
   where
    tasks = if _showDone state then [] else filterByDone False (_tasks state)
    doneTasks     = filterByDone True (_tasks state)
    nextNumber    = generateNumber doneTasks
    maybeTask     = readMaybe number >>= flip findByNumber tasks
    maybeDoneTask = readMaybe number >>= flip findByNumber doneTasks

setContext time args = SetContext time showDone ctx
 where
  showDone = "done" `elem` args
  ctx      = filter startsByPlus args

type Subscriber = ResponseType -> Command -> IO ()
notify :: ResponseType -> Command -> [Subscriber] -> IO ()
notify rtype command = foldr (\sub _ -> sub rtype command) (return ())

logger :: Subscriber
logger rtype event = case event of
  CreateTask  _ _ n _ _     -> printAction rtype n "created"
  UpdateTask  _ _ n _ _     -> printAction rtype n "updated"
  ReplaceTask _ _ n _ _     -> printAction rtype n "replaced"
  StartTask _ _ n           -> printAction rtype n "started"
  StopTask  _ _ n           -> printAction rtype n "stopped"
  MarkAsDoneTask _ _ n _    -> printAction rtype n "done"
  DeleteTask _ _        n   -> printAction rtype n "deleted"
  SetContext _ showDone ctx -> printCtx rtype showDone ctx
  Error cmd msg             -> printErr rtype $ cmd ++ ": " ++ msg
 where
  printAction rtype n action = printMsg rtype msg
    where msg = "task [" ++ show n ++ "] " ++ action
  printCtx rtype showDone ctx = printMsg rtype msg
   where
    ctxStr = unwords ([ "done" | showDone ] ++ ctx)
    msg    = "context "
      ++ if null ctxStr then "cleared" else "[" ++ ctxStr ++ "] set"
