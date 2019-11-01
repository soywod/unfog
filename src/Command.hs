module Command where

import           Data.List
import           Data.Time
import           Text.Read

import           Store
import           State
import           Task
import           Event
import           Utils

type Subscriber = Command -> IO ()

data Command
  = AddTask UTCTime Id Desc [Tag]
  | EditTask UTCTime Id Desc [Tag]
  | StartTask UTCTime Id
  | StopTask UTCTime Id
  | MarkAsDoneTask UTCTime Id Id
  | DeleteTask UTCTime Id
  | SetContext UTCTime [String]
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
  notify command subscribers
  write events

parseArgs :: UTCTime -> State -> [String] -> Command
parseArgs time state args = case args of
  ("add"     : args) -> addTask time state args
  ("edit"    : args) -> editTask time state args
  ("start"   : args) -> startTask time state args
  ("stop"    : args) -> stopTask time state args
  ("done"    : args) -> markAsDoneTask time state args
  ("delete"  : args) -> deleteTask time state args
  ("context" : args) -> SetContext time args
  (command   : _   ) -> Error command "command not found"
  []                 -> Error "" "command missing"

execute :: State -> Command -> [Event]
execute state command = case command of
  AddTask  time id desc tags    -> [TaskAdded time id desc tags]
  EditTask time id desc tags    -> [TaskEdited time id desc tags]
  StartTask time id             -> [TaskStarted time id]
  StopTask  time id             -> [TaskStopped time id]
  MarkAsDoneTask time id nextId -> [TaskMarkedAsDone time id nextId]
  DeleteTask time id            -> [TaskDeleted time id]
  SetContext time context       -> [ContextSet time context]
  Error      _    _             -> []
  NoOp                          -> []

addTask time state args = case args of
  []   -> Error "add" "missing desc"
  args -> AddTask time id desc tags
   where
    id   = generateId $ map _id $ filter (not . _done) $ _tasks state
    desc = unwords $ filter (not . startsByPlus) args
    tags = map tail $ filter startsByPlus args

editTask time state args = case args of
  []          -> Error "edit" "missing id"
  [_        ] -> Error "edit" "missing arg"
  (id : args) -> case readMaybe id >>= flip findById (_tasks state) of
    Nothing   -> Error "edit" "task not found"
    Just task -> EditTask time id desc tags
     where
      id   = _id task
      desc = unwords $ filter (not . startsByPlus) args
      tags = map tail $ filter startsByPlus args

startTask time state args = case args of
  []       -> Error "start" "missing id"
  (id : _) -> case readMaybe id >>= flip findById (_tasks state) of
    Nothing   -> Error "start" "task not found"
    Just task -> if _active task
      then Error "start" "task already started"
      else StartTask time $ _id task

stopTask time state args = case args of
  []       -> Error "stop" "missing id"
  (id : _) -> case readMaybe id >>= flip findById (_tasks state) of
    Nothing   -> Error "stop" "task not found"
    Just task -> if _active task
      then StopTask time $ _id task
      else Error "stop" "task already stopped"

markAsDoneTask time state args = case args of
  []       -> Error "done" "missing id"
  (id : _) -> case readMaybe id >>= flip findById (_tasks state) of
    Nothing   -> Error "done" "task not found"
    Just task -> if _done task
      then Error "done" "task already done"
      else MarkAsDoneTask time id nextId
     where
      id     = _id task
      nextId = generateId $ map _id $ filter _done $ _tasks state

deleteTask time state args = case args of
  []       -> Error "delete" "missing id"
  (id : _) -> case readMaybe id >>= flip findById (_tasks state) of
    Nothing   -> Error "delete" "task not found"
    Just task -> DeleteTask time $ _id task

notify :: Command -> [Subscriber] -> IO ()
notify command = foldr notify' (return ()) where notify' sub _ = sub command

logger :: Subscriber
logger command = case command of
  AddTask  _ id _ _          -> logAction id "added"
  EditTask _ id _ _          -> logAction id "edited"
  StartTask _ id             -> logAction id "started"
  StopTask  _ id             -> logAction id "stopped"
  MarkAsDoneTask _ id _      -> logAction id "done"
  DeleteTask _       id      -> logAction id "deleted"
  SetContext _       context -> logContext context
  Error      command message -> elog command message
 where
  logAction id event = putStrLn $ "unfog: task [" ++ show id ++ "] " ++ event
  logContext context =
    putStrLn $ "unfog: context " ++ if null context then "cleared" else "set"
