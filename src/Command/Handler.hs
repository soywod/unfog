module Command.Handler where

import Arg.Parser
import Data.Time
import Data.UUID.V4
import Event
import State
import Task
import Prelude hiding (id)

handleCommand :: Command -> IO ()
handleCommand (Add desc) = do
  state <- rebuild <$> readEvents
  now <- getCurrentTime
  id <- nextRandom
  writeEvents $ createTask state now id desc

createTask :: State -> UTCTime -> Id -> Desc -> [Event]
createTask state now id desc = [TaskCreated now id desc (getContext state)]
