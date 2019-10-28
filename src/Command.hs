module Command where

import           Data.List
import           Data.Time

import qualified Store
import           State
import           Task
import           Event

data Command = AddTask UTCTime Task deriving (Show, Read)

handle :: [String] -> IO ()
handle args = event >>= Store.writeEvent
 where
  state   = State.applyAll <$> Store.readAllEvents
  command = parseArgs <$> getCurrentTime <*> state <*> return args
  event   = execute <$> state <*> command

parseArgs :: UTCTime -> State -> [String] -> Command
parseArgs time state ("add" : args) =
  AddTask time Task { _id = generateId state, _desc = unwords args }

execute :: State -> Command -> Event
execute state (AddTask time task) = TaskAdded time task

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
