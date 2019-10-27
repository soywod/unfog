module Command where

import           Prelude                 hiding ( id )

import qualified Store
import           State
import           Task
import           Event

newtype Command = AddTask Task deriving (Show, Read)

handle :: [String] -> IO ()
handle args = event >>= Store.writeEvent
 where
  state   = State.applyAll <$> Store.readAllEvents
  command = return $ parseArgs args
  event   = execute <$> state <*> command

parseArgs :: [String] -> Command
parseArgs ("add" : args) = AddTask Task { id = 0, desc = unwords args }

execute :: State -> Command -> Event
execute state (AddTask task) = TaskAdded task
