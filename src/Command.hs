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
handle args = event >>= writeEvent
 where
  state   = apply <$> readEvents
  command = return $ parseArgs args
  event   = execute <$> state <*> command

execute :: State -> Command -> Event
execute state (AddTask task) = TaskAdded task
