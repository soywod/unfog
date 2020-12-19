module Unfog.Query.Handler where

import Control.Applicative ((<|>))
import Data.Maybe (isJust, isNothing)
import Data.Time (UTCTime, getCurrentTime)
import qualified Unfog.Arg.Types as Arg
import Unfog.Query.Types (Query)
import qualified Unfog.Query.Types as Query
import Unfog.Response
import Unfog.State (State (..))
import qualified Unfog.State as State
import qualified Unfog.Store as Store
import Unfog.Task
import Unfog.Worktime

showTasks :: UTCTime -> State -> Arg.DueInOpt -> Arg.DoneOpt -> Arg.DeletedOpt -> Arg.JsonOpt -> Query
showTasks now (State ctx tasks) dueInOpt doneOpt deletedOpt jsonOpt = Query.ShowTasks now rtype idLength ctx tasks'
  where
    rtype = parseResponseType jsonOpt
    matchDoneOpt = (==) doneOpt . isJust . getDone
    matchDeletedOpt = (==) deletedOpt . isJust . getDeleted
    matchDueInOpt = matchDueIn now dueInOpt
    tasks' = filterWith [matchContext ctx, matchDoneOpt, matchDeletedOpt, matchDueInOpt] tasks
    idLength = getShortIdLength tasks

showTask :: UTCTime -> State -> Id -> Arg.JsonOpt -> Query
showTask now (State _ tasks) id jsonOpt = case findById id tasks of
  Nothing -> Query.Error rtype "task not found"
  Just task -> Query.ShowTask now rtype task
  where
    rtype = parseResponseType jsonOpt

showWtime :: UTCTime -> State -> Project -> Arg.FromOpt -> Arg.ToOpt -> Arg.MoreOpt -> Arg.JsonOpt -> Query
showWtime now (State ctx tasks) proj fromOpt toOpt moreOpt jsonOpt = Query.ShowWorktime now rtype moreOpt wtimes
  where
    ctx' = if isNothing proj then ctx else proj
    tasks' = filterWith [notDeleted, matchContext ctx'] tasks
    wtimes = buildWtimePerDay now idLength fromOpt toOpt tasks'
    rtype = parseResponseType jsonOpt
    idLength = getShortIdLength tasks

showStatus :: UTCTime -> State -> Arg.MoreOpt -> Arg.JsonOpt -> Query
showStatus now (State _ tasks) _ jsonOpt = Query.ShowStatus now rtype $ findFstActive tasks
  where
    rtype = parseResponseType jsonOpt

queryOfArg :: UTCTime -> State -> Arg.Query -> Query
queryOfArg now state (Arg.ShowTasks dueInOpt doneOpt deletedOpt jsonOpt) = showTasks now state dueInOpt doneOpt deletedOpt jsonOpt
queryOfArg now state (Arg.ShowTask id jsonOpt) = showTask now state id jsonOpt
queryOfArg now state (Arg.ShowWorktime proj fromOpt toOpt moreOpt jsonOpt) = showWtime now state proj fromOpt toOpt moreOpt jsonOpt
queryOfArg now state (Arg.ShowStatus moreOpt jsonOpt) = showStatus now state moreOpt jsonOpt

execute :: Query -> IO ()
execute (Query.ShowTasks now rtype idLength ctx tasks) = send rtype (TasksResponse now idLength ctx tasks)
execute (Query.ShowTask now rtype task) = send rtype (TaskResponse now task)
execute (Query.ShowWorktime now rtype moreOpt wtimes) = send rtype (WorktimeResponse now moreOpt wtimes)
execute (Query.ShowStatus now rtype task) = send rtype (StatusResponse now task)
execute (Query.Error rtype msg) = send rtype (ErrorResponse msg)

handle :: Arg.Query -> IO ()
handle arg = do
  now <- getCurrentTime
  state <- State.readCache <|> (State.rebuild <$> Store.readFile) <|> return State.new
  let query = queryOfArg now state arg
  execute query
