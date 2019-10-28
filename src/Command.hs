module Command where

import           Data.List                      ( sort )

import qualified Store
import           State
import           Task
import           Event

newtype Command = AddTask Task deriving (Show, Read)

handle :: [String] -> IO ()
handle args = event >>= Store.writeEvent
 where
  state   = State.applyAll <$> Store.readAllEvents
  command = parseArgs <$> state <*> return args
  event   = execute <$> state <*> command

parseArgs :: State -> [String] -> Command
parseArgs state ("add" : args) =
  AddTask Task { _id = generateId state, _desc = unwords args }

execute :: State -> Command -> Event
execute state (AddTask task) = TaskAdded task

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
