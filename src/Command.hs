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
  = CreateTask UTCTime Ref Id Pos Desc [Tag] Due
  | UpdateTask UTCTime Ref Id Pos Desc [Tag] Due
  | StartTask UTCTime Ref Id
  | StopTask UTCTime Ref Id
  | MarkAsDoneTask UTCTime Ref Id Id
  | DeleteTask UTCTime Ref Id
  | SetContext UTCTime Bool [String]
  | Error String String
  | NoOp
  deriving (Show, Read)

handle :: Parsec.ArgTree -> IO ()
handle args = do
  now  <- getCurrentTime
  evts <- readEvents
  let state       = applyEvents evts
  let command     = getCmd now args state
  let events      = execute state command
  let subscribers = [logger]
  writeEvents evts
  -- notify rtype command subscribers

execute :: State -> Command -> [Event]
execute state command = case command of
  CreateTask t r i p desc tags due -> [TaskCreated t r i p desc tags due]
  UpdateTask t r i p desc tags due -> [TaskUpdated t r i p desc tags due]
  StartTask t ref id               -> [TaskStarted t ref id]
  StopTask  t ref id               -> [TaskStopped t ref id]
  MarkAsDoneTask t ref _ id        -> [TaskMarkedAsDone t ref id]
  DeleteTask t ref      id         -> [TaskDeleted t ref id]
  SetContext t showDone context    -> [ContextSet t showDone context]
  Error _ _                        -> []
  NoOp                             -> []

getCmd :: UTCTime -> Parsec.ArgTree -> State -> Command
getCmd t args state = case Parsec._cmd args of
  "create" -> createTask t args state
  -- "update"  -> updateTask t args state
  -- "replace" -> replaceTask t args state
  -- "start"   -> startTask t args state
  -- "stop"    -> stopTask t args state
  -- "toggle"  -> toggleTask t args state
  -- "done"    -> markAsDoneTask t args state
  -- "delete"  -> deleteTask t args state args
  -- "remove"  -> removeTask t args state args
  -- "context" -> setContext t args
  -- (cmd : _) -> Error cmd "command not found"
  []       -> Error "" "command missing"

createTask :: UTCTime -> Parsec.ArgTree -> State -> Command
createTask t args state = CreateTask t ref id pos desc tags due
 where
  ref  = floor $ 1000 * utcTimeToPOSIXSeconds t :: Int
  id   = generateId $ filter (not . _done) $ _tasks state
  pos  = -1
  desc = Parsec._desc args
  tags = Parsec._tags args `union` _context state
  due  = Nothing

-- updateTask :: UTCTime -> Parsec.ArgTree -> State -> Command
-- updateTask t args state = case args of
--   []          -> Error "update" "missing id"
--   [_        ] -> Error "update" "missing args"
--   (id : args) -> case maybeTask of
--     Nothing   -> Error "update" "task not found"
--     Just task -> validate task
--    where
--     tasks     = filterByDone (_showDone state) (_tasks state)
--     maybeTask = readMaybe id >>= flip findById tasks
--     newDesc   = unwords $ filter (not . startsByPlus) args
--     nextDesc  = if newDesc == "" then maybe "" _desc maybeTask else newDesc
--     newTags   = filter startsByPlus args
--     nextTags  = union newTags $ maybe [] _tags maybeTask
--     validate task | _done task = Error "update" "task already done"
--                   | otherwise  = update task
--     update task =
--       let ref = _ref task
--           id  = _id task
--           pos = -1
--           due = Nothing
--       in  UpdateTask t ref id pos nextDesc nextTags due

-- replaceTask t state args = case args of
--   []          -> Error "replace" "missing id"
--   [_        ] -> Error "replace" "missing args"
--   (id : args) -> case maybeTask of
--     Nothing   -> Error "replace" "task not found"
--     Just task -> validate task
--    where
--     tasks     = filterByDone (_showDone state) (_tasks state)
--     maybeTask = readMaybe id >>= flip findById tasks
--     nextDesc  = unwords $ filter (not . startsByPlus) args
--     nextTags  = filter startsByPlus args
--     validate task | _done task = Error "replace" "task already done"
--                   | otherwise  = update task
--     update task =
--       let ref = _ref task
--           id  = _id task
--           pos = -1
--           due = Nothing
--       in  UpdateTask t ref id pos nextDesc nextTags due

-- startTask t state args = case args of
--   []       -> Error "start" "missing id"
--   (id : _) -> case maybeTask of
--     Nothing   -> Error "start" "task not found"
--     Just task -> validate task
--    where
--     tasks     = filterByDone (_showDone state) (_tasks state)
--     maybeTask = readMaybe id >>= flip findById tasks
--     validate task | _done task   = Error "start" "task already done"
--                   | _active task = Error "start" "task already started"
--                   | otherwise    = StartTask t (_ref task) (_id task)

-- stopTask t state args = case args of
--   []       -> Error "stop" "missing id"
--   (id : _) -> case maybeTask of
--     Nothing   -> Error "stop" "task not found"
--     Just task -> validate task
--    where
--     tasks     = filterByDone (_showDone state) (_tasks state)
--     maybeTask = readMaybe id >>= flip findById tasks
--     validate task | _done task           = Error "stop" "task already done"
--                   | (not . _active) task = Error "stop" "task already stopped"
--                   | otherwise            = StopTask t (_ref task) (_id task)

-- toggleTask t state args = case args of
--   []       -> Error "toggle" "missing id"
--   (id : _) -> case maybeTask of
--     Nothing   -> Error "toggle" "task not found"
--     Just task -> validate task
--    where
--     tasks     = filterByDone (_showDone state) (_tasks state)
--     maybeTask = readMaybe id >>= flip findById tasks
--     validate task | _done task   = Error "toggle" "task already done"
--                   | _active task = StopTask t (_ref task) (_id task)
--                   | otherwise    = StartTask t (_ref task) (_id task)

-- markAsDoneTask t state args = case args of
--   []       -> Error "done" "missing id"
--   (id : _) -> case maybeTask of
--     Nothing   -> Error "done" "task not found"
--     Just task -> validate task
--    where
--     tasks      = filterByDone (_showDone state) (_tasks state)
--     maybeTask  = readMaybe id >>= flip findById tasks
--     nextNumber = generateId $ filter _done $ _tasks state
--     validate task
--       | _done task = Error "done" "task already done"
--       | otherwise  = MarkAsDoneTask t (_ref task) (_id task) nextNumber

-- deleteTask t state args = case args of
--   []       -> Error "delete" "missing id"
--   (id : _) -> case maybeTask of
--     Nothing   -> Error "delete" "task not found"
--     Just task -> DeleteTask t (_ref task) (_id task)
--    where
--     tasks     = filterByDone (_showDone state) (_tasks state)
--     maybeTask = readMaybe id >>= flip findById tasks

-- removeTask t state args = case args of
--   []       -> Error "remove" "missing id"
--   (id : _) -> case maybeTask of
--     Just task -> MarkAsDoneTask t (_ref task) (_id task) nextNumber
--     Nothing   -> case maybeDoneTask of
--       Just task -> DeleteTask t (_ref task) (_id task)
--       Nothing   -> Error "remove" "task not found"
--    where
--     tasks = if _showDone state then [] else filterByDone False (_tasks state)
--     doneTasks     = filterByDone True (_tasks state)
--     nextNumber    = generateId doneTasks
--     maybeTask     = readMaybe id >>= flip findById tasks
--     maybeDoneTask = readMaybe id >>= flip findById doneTasks

setContext t args = SetContext t showDone ctx
 where
  showDone = "done" `elem` args
  ctx      = filter startsByPlus args

type Subscriber = ResponseType -> Command -> IO ()
notify :: ResponseType -> Command -> [Subscriber] -> IO ()
notify rtype command = foldr (\sub _ -> sub rtype command) (return ())

logger :: Subscriber
logger rtype event = case event of
  CreateTask _ _ id _ _ _ _ -> printAction rtype id "created"
  UpdateTask _ _ id _ _ _ _ -> printAction rtype id "updated"
  StartTask _ _ id          -> printAction rtype id "started"
  StopTask  _ _ id          -> printAction rtype id "stopped"
  MarkAsDoneTask _ _ id _   -> printAction rtype id "done"
  DeleteTask _ _        id  -> printAction rtype id "deleted"
  SetContext _ showDone ctx -> printCtx rtype showDone ctx
  Error cmd msg             -> printErr rtype $ cmd ++ ": " ++ msg
 where
  printAction rtype id action =
    let msg = "task [" ++ show id ++ "] " ++ action in printMsg rtype msg

  printCtx rtype showDone ctx =
    let
      ctxStr = unwords ([ "done" | showDone ] ++ ctx)
      msg    = "context "
        ++ if null ctxStr then "cleared" else "[" ++ ctxStr ++ "] set"
    in
      printMsg rtype msg
