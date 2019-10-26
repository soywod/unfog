module Command where

import           Prelude                 hiding ( id )

import           State
import           Store
import           Task
import           Event

newtype Command = AddTask Task deriving (Show, Read)

parseArgs :: [String] -> Command
parseArgs ("add" : args) = AddTask Task { id = 0, desc = unwords args }

handle :: [String] -> IO ()
handle args = do
  events <- readEvents
  let state   = foldl State.apply State.new events
  let command = parseArgs args
  let event   = execute state command
  writeEvent event

execute :: State -> Command -> Event
execute state (AddTask task) = TaskAdded task
