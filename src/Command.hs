{-# LANGUAGE NamedFieldPuns #-}
module Command where

import           Data.List
import           Data.Time

import qualified Store
import           State
import           Task
import           Event
import           Utils.Predicate                ( startsByPlus )

data Command = NoOp | AddTask UTCTime Task | EditTask UTCTime Task deriving (Show, Read)

handle :: [String] -> IO ()
handle args = events >>= Store.writeEvents
 where
  state   = State.applyAll <$> Store.readAllEvents
  command = parseArgs <$> getCurrentTime <*> state <*> return args
  events  = execute <$> state <*> command

parseArgs :: UTCTime -> State -> [String] -> Command
parseArgs time state ("add"  : args) = addTask time state args
parseArgs time state ("edit" : args) = editTask time state args

addTask time state args = AddTask time Task { _id, _desc, _tags }
 where
  _id   = generateId state
  _desc = unwords $ filter (not . startsByPlus) args
  _tags = map tail $ filter startsByPlus args

editTask time state args = case args of
  [_        ] -> Command.NoOp
  (id : args) -> case find ((== read id) . _id) (_tasks state) of
    Nothing   -> Command.NoOp
    Just task -> EditTask time Task { _id = _id task, _desc, _tags }
     where
      _desc = unwords $ filter (not . startsByPlus) args
      _tags = map tail $ filter startsByPlus args

execute :: State -> Command -> [Event]
execute state (AddTask  time task) = [TaskAdded time task]
execute state (EditTask time task) = [TaskEdited time task]
execute state NoOp                 = []

generateId :: State -> Int
generateId state = generateId' currIds genIds
 where
  currIds = sort $ map _id $ _tasks state
  genIds  = [1 ..]

generateId' :: [Int] -> [Int] -> Int
generateId' [] []          = 1
generateId' [] (genId : _) = genId
generateId' (currId : currIds) (genId : genIds)
  | currId == genId = generateId' currIds genIds
  | otherwise       = genId
