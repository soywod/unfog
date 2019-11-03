module Command where

import           Data.List
import           Data.Time
import           Text.Read
import           Data.Time.Clock.POSIX

import           Store
import           State
import           Task
import           Event
import           Utils

type Subscriber = Command -> IO ()

data Command
  = AddTask UTCTime Id Number Desc [Tag]
  | EditTask UTCTime Id Number Desc [Tag]
  | StartTask UTCTime Id Number
  | StopTask UTCTime Id Number
  | MarkAsDoneTask UTCTime Id Number
  | DeleteTask UTCTime Id Number
  | SetContext UTCTime Bool [String]
  | Error String String
  | NoOp
  deriving (Show, Read)

handle :: [String] -> IO ()
handle args = do
  now   <- getCurrentTime
  state <- State.applyAll <$> Store.readAll
  let command     = parseArgs now state args
  let events      = execute state command
  let subscribers = [logger]
  write events
  notify command subscribers

execute :: State -> Command -> [Event]
execute state command = case command of
  AddTask  time id number desc tags -> [TaskAdded time id number desc tags]
  EditTask time id number desc tags -> [TaskEdited time id number desc tags]
  StartTask time id number          -> [TaskStarted time id number]
  StopTask time id number           -> [TaskStopped time id number]
  MarkAsDoneTask time id number     -> [TaskMarkedAsDone time id number]
  DeleteTask time id number         -> [TaskDeleted time id number]
  SetContext time showDone context  -> [ContextSet time showDone context]
  Error _ _                         -> []
  NoOp                              -> []

parseArgs :: UTCTime -> State -> [String] -> Command
parseArgs time state args = case args of
  ("add"     : args) -> addTask time state args
  ("edit"    : args) -> editTask time state args
  ("start"   : args) -> startTask time state args
  ("stop"    : args) -> stopTask time state args
  ("done"    : args) -> markAsDoneTask time state args
  ("delete"  : args) -> deleteTask time state args
  ("context" : args) -> setContext time args
  (command   : _   ) -> Error command "command not found"
  []                 -> Error "" "command missing"

addTask time state args = case args of
  []   -> Error "add" "missing desc"
  args -> AddTask time id number desc tags
   where
    id     = floor $ 1000 * utcTimeToPOSIXSeconds time :: Int
    number = generateNumber $ filter (not . _done) $ _tasks state
    desc   = unwords $ filter (not . startsByPlus) args
    tags   = filter startsByPlus args `union` _context state

editTask time state args = case args of
  []              -> Error "edit" "missing id"
  [_            ] -> Error "edit" "missing arg"
  (number : args) -> case maybeTask of
    Nothing   -> Error "edit" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks
    newDesc   = unwords $ filter (not . startsByPlus) args
    nextDesc  = if newDesc == "" then maybe "" _desc maybeTask else newDesc
    newTags   = filter startsByPlus args
    nextTags  = union newTags $ maybe [] _tags maybeTask
    validate task
      | _done task = Error "start" "task already done"
      | otherwise  = EditTask time (_id task) (_number task) nextDesc nextTags

startTask time state args = case args of
  []           -> Error "start" "missing id"
  (number : _) -> case maybeTask of
    Nothing   -> Error "start" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks
    validate task | _active task = Error "start" "task already started"
                  | _done task   = Error "start" "task already done"
                  | otherwise    = StartTask time (_id task) (_number task)

stopTask time state args = case args of
  []           -> Error "start" "missing id"
  (number : _) -> case maybeTask of
    Nothing   -> Error "start" "task not found"
    Just task -> validate task
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks
    validate task | not $ _active task = Error "stop" "task already stopped"
                  | _done task         = Error "stop" "task already done"
                  | otherwise          = StopTask time (_id task) (_number task)

markAsDoneTask time state args = case args of
  []           -> Error "done" "missing id"
  (number : _) -> case maybeTask of
    Nothing   -> Error "done" "task not found"
    Just task -> validate task
   where
    tasks      = filterByDone (_showDone state) (_tasks state)
    maybeTask  = readMaybe number >>= flip findByNumber tasks
    nextNumber = generateNumber $ filter _done $ _tasks state
    validate task | _done task = Error "done" "task already done"
                  | otherwise  = MarkAsDoneTask time (_id task) nextNumber

deleteTask time state args = case args of
  []           -> Error "delete" "missing id"
  (number : _) -> case maybeTask of
    Nothing   -> Error "delete" "task not found"
    Just task -> DeleteTask time (_id task) (_number task)
   where
    tasks     = filterByDone (_showDone state) (_tasks state)
    maybeTask = readMaybe number >>= flip findByNumber tasks

setContext time args = SetContext time showDone context
 where
  showDone = "done" `elem` args
  context  = filter startsByPlus args

notify :: Command -> [Subscriber] -> IO ()
notify command = foldr (\sub _ -> sub command) (return ())

logger :: Subscriber
logger event = case event of
  AddTask  _ _ number _ _           -> logAction number "added"
  EditTask _ _ number _ _           -> logAction number "edited"
  StartTask      _ _        number  -> logAction number "started"
  StopTask       _ _        number  -> logAction number "stopped"
  MarkAsDoneTask _ _        number  -> logAction number "done"
  DeleteTask     _ _        number  -> logAction number "deleted"
  SetContext     _ showDone context -> logContext showDone context
  Error command message             -> elog command message
 where
  logAction n event = putStrLn $ "unfog: task [" ++ show n ++ "] " ++ event
  logContext showDone context =
    let contextStr = unwords ([ "done" | showDone ] ++ context)
    in  putStrLn $ "unfog: context " ++ if null contextStr
          then "cleared"
          else "[" ++ contextStr ++ "] set"
