module Command where

import           Data.List
import           Data.Time

import qualified Store
import           State
import           Task
import           Event
import           Utils.Predicate                ( startsByPlus )

data Command
  = NoOp
  | AddTask UTCTime Id Desc [Tag]
  | EditTask UTCTime Id Desc [Tag]
  | StartTask UTCTime Id
  | StopTask UTCTime Id
  | MarkAsDoneTask UTCTime Id Id
  deriving (Show, Read)

handle :: [String] -> IO ()
handle args = events >>= Store.writeEvents
 where
  state   = State.applyAll <$> Store.readAllEvents
  command = parseArgs <$> getCurrentTime <*> state <*> return args
  events  = execute <$> state <*> command

parseArgs :: UTCTime -> State -> [String] -> Command
parseArgs time state ("add"   : args) = addTask time state args
parseArgs time state ("edit"  : args) = editTask time state args
parseArgs time state ("start" : args) = startTask time state args
parseArgs time state ("stop"  : args) = stopTask time state args
parseArgs time state ("done"  : args) = markAsDoneTask time state args

addTask time state args = AddTask time id desc tags
 where
  id   = generateId $ filter (not . _done) (_tasks state)
  desc = unwords $ filter (not . startsByPlus) args
  tags = map tail $ filter startsByPlus args

editTask time state args = case args of
  []          -> NoOp
  [_        ] -> NoOp
  (id : args) -> case findById (read id) (_tasks state) of
    Nothing   -> NoOp
    Just task -> EditTask time id desc tags
     where
      id   = _id task
      desc = unwords $ filter (not . startsByPlus) args
      tags = map tail $ filter startsByPlus args

startTask time state args = case args of
  []       -> NoOp
  (id : _) -> case findById (read id) (_tasks state) of
    Nothing   -> NoOp
    Just task -> if _active task then NoOp else StartTask time $ _id task

stopTask time state args = case args of
  []       -> NoOp
  (id : _) -> case findById (read id) (_tasks state) of
    Nothing   -> NoOp
    Just task -> if _active task then StopTask time $ _id task else NoOp

markAsDoneTask time state args = case args of
  []       -> NoOp
  (id : _) -> case findById (read id) (_tasks state) of
    Nothing   -> NoOp
    Just task -> if _done task then NoOp else MarkAsDoneTask time id nextId
     where
      id     = _id task
      nextId = generateId $ filter _done (_tasks state)

execute :: State -> Command -> [Event]
execute state (AddTask  time id desc tags) = [TaskAdded time id desc tags]
execute state (EditTask time id desc tags) = [TaskEdited time id desc tags]
execute state (StartTask time id         ) = [TaskStarted time id]
execute state (StopTask  time id         ) = [TaskStopped time id]
execute state (MarkAsDoneTask time id nextId) =
  [TaskMarkedAsDone time id nextId]
execute state NoOp = []

generateId :: [Task] -> Int
generateId tasks = generateId' currIds genIds
 where
  currIds = sort $ map _id tasks
  genIds  = [1 ..]

generateId' :: [Int] -> [Int] -> Int
generateId' [] []          = 1
generateId' [] (genId : _) = genId
generateId' (currId : currIds) (genId : genIds)
  | currId == genId = generateId' currIds genIds
  | otherwise       = genId
