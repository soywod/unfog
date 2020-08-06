module Query where

import ArgOptions
import qualified ArgParser as Arg
import Data.List
import Data.Time
import Event
import Response
import State
import Task
import Worktime

data Query
  = ShowTasks UTCTime ResponseType [Task]
  | ShowTask UTCTime ResponseType Task
  | ShowWtime UTCTime ResponseType [DailyWorktime]
  | ShowStatus UTCTime ResponseType (Maybe Task)
  | Error ResponseType String
  deriving (Show, Read)

handle :: Arg.Query -> IO ()
handle arg = do
  now <- getCurrentTime
  state <- rebuild <$> readEvents
  let query = parseQuery now state arg
  execute query

parseQuery :: UTCTime -> State -> Arg.Query -> Query
parseQuery now state (Arg.List onlyIdsOpt onlyTagsOpt jsonOpt) = showTasks now state onlyIdsOpt onlyTagsOpt jsonOpt
parseQuery now state (Arg.Info id jsonOpt) = showTask now state id jsonOpt
parseQuery now state (Arg.Wtime tags fromOpt toOpt moreOpt jsonOpt) = showWtime now state tags fromOpt toOpt moreOpt jsonOpt
parseQuery now state (Arg.Status moreOpt jsonOpt) = showStatus now state moreOpt jsonOpt

execute :: Query -> IO ()
execute (ShowTasks now rtype tasks) = send rtype (TasksResponse now tasks)
execute (ShowTask now rtype task) = send rtype (TaskResponse now task)
execute (ShowWtime now rtype wtimes) = send rtype (WtimeResponse now wtimes)
execute (ShowStatus now rtype task) = send rtype (StatusResponse now task)
execute (Error rtype msg) = send rtype (ErrorResponse msg)

showTasks :: UTCTime -> State -> OnlyIdsOpt -> OnlyTagsOpt -> JsonOpt -> Query
showTasks now state onlyIdsOpt onlyTagsOpt jsonOpt
  | onlyIdsOpt = ShowTasks now rtype (getVisibleTasks state) -- TODO
  | onlyTagsOpt = ShowTasks now rtype (getVisibleTasks state) -- TODO
  | otherwise = ShowTasks now rtype (getVisibleTasks state)
  where
    rtype = parseResponseType jsonOpt

showTask :: UTCTime -> State -> Id -> JsonOpt -> Query
showTask now state id jsonOpt = case findById id (getTasks state) of
  Nothing -> Error rtype "task not found"
  Just task -> ShowTask now rtype task
  where
    rtype = parseResponseType jsonOpt

showWtime :: UTCTime -> State -> [Tag] -> FromOpt -> ToOpt -> MoreOpt -> JsonOpt -> Query
showWtime now state tags fromOpt toOpt moreOpt jsonOpt = ShowWtime now rtype wtimes
  where
    tasks = getTasks state
    wtimes = buildWtimePerDay now fromOpt toOpt tasks
    rtype = parseResponseType jsonOpt

showStatus :: UTCTime -> State -> MoreOpt -> JsonOpt -> Query
showStatus now state moreOpt jsonOpt = ShowStatus now rtype $ findFstActiveTask (getTasks state)
  where
    rtype = parseResponseType jsonOpt
