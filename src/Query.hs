module Query where

import ArgOptions
import qualified ArgParser as Arg
import Control.Applicative ((<|>))
import Data.Maybe (isJust, isNothing)
import Data.Time (TimeZone, UTCTime, getCurrentTime, getCurrentTimeZone)
import Response
import State (State (..))
import qualified State (getTasks, new, readFile, rebuild)
import qualified Store (readFile)
import Task
import Worktime

data Query
  = ShowTasks UTCTime IdLength ResponseType Project [Task]
  | ShowTask UTCTime ResponseType Task
  | ShowWorktime UTCTime ResponseType MoreOpt [DailyWorktime]
  | ShowStatus UTCTime ResponseType (Maybe Task)
  | Error ResponseType String
  deriving (Show, Read, Eq)

handle :: Arg.Query -> IO ()
handle arg = do
  now <- getCurrentTime
  state <- State.readFile <|> (State.rebuild <$> Store.readFile) <|> return State.new
  let query = parseQuery now state arg
  execute query

parseQuery :: UTCTime -> State -> Arg.Query -> Query
parseQuery now state (Arg.ShowTasks doneOpt deletedOpt jsonOpt) = showTasks now state doneOpt deletedOpt jsonOpt
parseQuery now state (Arg.ShowTask id jsonOpt) = showTask now state id jsonOpt
parseQuery now state (Arg.ShowWorktime proj fromOpt toOpt moreOpt jsonOpt) = showWtime now state proj fromOpt toOpt moreOpt jsonOpt
parseQuery now state (Arg.ShowStatus moreOpt jsonOpt) = showStatus now state moreOpt jsonOpt

execute :: Query -> IO ()
execute (ShowTasks now idLength rtype ctx tasks) = send rtype (TasksResponse now idLength ctx tasks)
execute (ShowTask now rtype task) = send rtype (TaskResponse now task)
execute (ShowWorktime now rtype moreOpt wtimes) = send rtype (WorktimeResponse now moreOpt wtimes)
execute (ShowStatus now rtype task) = send rtype (StatusResponse now task)
execute (Error rtype msg) = send rtype (ErrorResponse msg)

showTasks :: UTCTime -> State -> DoneOpt -> DeletedOpt -> JsonOpt -> Query
showTasks now (State ctx tasks) doneOpt deletedOpt jsonOpt = ShowTasks now idLength rtype ctx tasks'
  where
    rtype = parseResponseType jsonOpt
    filters = [matchContext ctx] ++ [(==) doneOpt . isJust . getDone] ++ [(==) deletedOpt . isJust . getDeleted]
    tasks' = filterWith filters tasks
    idLength = getShortIdLength tasks

showTask :: UTCTime -> State -> Id -> JsonOpt -> Query
showTask now (State _ tasks) id jsonOpt = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> ShowTask now rtype task
  where
    rtype = parseResponseType jsonOpt

showWtime :: UTCTime -> State -> Project -> FromOpt -> ToOpt -> MoreOpt -> JsonOpt -> Query
showWtime now (State ctx tasks) proj fromOpt toOpt moreOpt jsonOpt = ShowWorktime now rtype moreOpt wtimes
  where
    ctx' = if isNothing proj then ctx else proj
    tasks' = filterWith [notDeleted, matchContext ctx'] tasks
    wtimes = buildWtimePerDay now idLength fromOpt toOpt tasks'
    rtype = parseResponseType jsonOpt
    idLength = getShortIdLength tasks

showStatus :: UTCTime -> State -> MoreOpt -> JsonOpt -> Query
showStatus now (State _ tasks) moreOpt jsonOpt = ShowStatus now rtype $ findFstActive tasks
  where
    rtype = parseResponseType jsonOpt
