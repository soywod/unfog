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

type IdLength = Int

data Query
  = ShowTasks UTCTime IdLength ResponseType Project [Task]
  | ShowTask UTCTime IdLength ResponseType Task
  | ShowWtime UTCTime ResponseType MoreOpt [DailyWorktime]
  | ShowStatus UTCTime ResponseType (Maybe Task)
  | Error ResponseType String
  deriving (Show, Read, Eq)

handle :: Arg.Query -> IO ()
handle arg = do
  now <- getCurrentTime
  state <- State.readFile <|> (State.rebuild <$> Store.readFile) <|> return State.new
  let idLength = getShortIdLength $ State.getTasks state
  let query = parseQuery now idLength state arg
  execute query

parseQuery :: UTCTime -> IdLength -> State -> Arg.Query -> Query
parseQuery now idLength state (Arg.List jsonOpt) = showTasks now idLength state jsonOpt
parseQuery now idLength state (Arg.Info id jsonOpt) = showTask now idLength state id jsonOpt
parseQuery now _idLength state (Arg.Wtime proj fromOpt toOpt moreOpt jsonOpt) = showWtime now state proj fromOpt toOpt moreOpt jsonOpt
parseQuery now _idLength state (Arg.Status moreOpt jsonOpt) = showStatus now state moreOpt jsonOpt

execute :: Query -> IO ()
execute (ShowTasks now idLength rtype ctx tasks) = send rtype (TasksResponse now idLength ctx tasks)
execute (ShowTask now idLength rtype task) = send rtype (TaskResponse now idLength task)
execute (ShowWtime now rtype moreOpt wtimes) = send rtype (WtimeResponse now moreOpt wtimes)
execute (ShowStatus now rtype task) = send rtype (StatusResponse now task)
execute (Error rtype msg) = send rtype (ErrorResponse msg)

showTasks :: UTCTime -> IdLength -> State -> JsonOpt -> Query
showTasks now idLength (State ctx tasks) jsonOpt = ShowTasks now idLength rtype ctx tasks'
  where
    rtype = parseResponseType jsonOpt
    tasks' = filterWith [notDone, notDeleted, matchContext ctx] tasks

showTask :: UTCTime -> IdLength -> State -> Id -> JsonOpt -> Query
showTask now idLength (State _ tasks) id jsonOpt = case findById id tasks of
  Nothing -> Error rtype "task not found"
  Just task -> ShowTask now idLength rtype task
  where
    rtype = parseResponseType jsonOpt

showWtime :: UTCTime -> State -> Project -> FromOpt -> ToOpt -> MoreOpt -> JsonOpt -> Query
showWtime now (State ctx tasks) proj fromOpt toOpt moreOpt jsonOpt = ShowWtime now rtype moreOpt wtimes
  where
    ctx' = if isNothing proj then ctx else proj
    tasks' = filterWith [notDeleted, matchContext ctx'] tasks
    wtimes = buildWtimePerDay now fromOpt toOpt tasks'
    rtype = parseResponseType jsonOpt

showStatus :: UTCTime -> State -> MoreOpt -> JsonOpt -> Query
showStatus now (State _ tasks) moreOpt jsonOpt = ShowStatus now rtype $ findFstActive tasks
  where
    rtype = parseResponseType jsonOpt
