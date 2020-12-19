module Unfog.Query.Types where

import Data.Time (UTCTime)
import qualified Unfog.Arg.Types as Arg
import Unfog.Response (ResponseType)
import Unfog.Task (IdLength, Project, Task)
import Unfog.Worktime (DailyWorktime)

data Query
  = ShowTasks UTCTime ResponseType IdLength Project [Task]
  | ShowTask UTCTime ResponseType Task
  | ShowWorktime UTCTime ResponseType Arg.MoreOpt [DailyWorktime]
  | ShowStatus UTCTime ResponseType (Maybe Task)
  | Error ResponseType String
  deriving (Show, Read, Eq)
