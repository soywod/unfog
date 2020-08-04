module Query where

import Arg.Options
import qualified Arg.Parser as Arg
import Data.List
import Data.Time
import Event
import Response
import State
import System.Process (system)
import Task
import Worktime

data Query
  = ShowTasks UTCTime ResponseType [Task]
  | ShowTask UTCTime ResponseType Task
  | ShowWtime UTCTime ResponseType [DailyWorktime]
  | ShowStatus UTCTime ResponseType (Maybe Task)
  | ShowVersion ResponseType String
  | DoUpgrade
  | Error ResponseType String String
  deriving (Show, Read)

handle :: Arg.Query -> IO ()
handle arg = do
  now <- getCurrentTime
  state <- rebuild <$> readEvents
  let query = parseQuery now state arg
  execute query

parseQuery :: UTCTime -> State -> Arg.Query -> Query
parseQuery now state (Arg.List onlyIdsOpt onlyTagsOpt moreOpt jsonOpt) = showTasks now state onlyIdsOpt onlyTagsOpt moreOpt jsonOpt
parseQuery now state (Arg.Info id moreOpt jsonOpt) = showTask now state id jsonOpt
parseQuery now state (Arg.Wtime tags fromOpt toOpt moreOpt jsonOpt) = showWtime now state tags fromOpt toOpt moreOpt jsonOpt
parseQuery now state (Arg.Status moreOpt jsonOpt) = showStatus now state moreOpt jsonOpt
parseQuery _ state (Arg.Version jsonOpt) = showVersion jsonOpt
parseQuery _ state Arg.Upgrade = DoUpgrade

execute :: Query -> IO ()
execute (ShowTasks now rtype tasks) = send rtype (TasksResponse now tasks)
execute (ShowTask now rtype task) = send rtype (TaskResponse now task)
execute (ShowWtime now rtype wtimes) = send rtype (WtimeResponse now wtimes)
execute (ShowStatus now rtype task) = send rtype (StatusResponse now task)
execute (ShowVersion rtype version) = send rtype (VersionResponse version)
execute (DoUpgrade) = doUpgrade
execute (Error rtype query err) = send rtype (ErrorResponse query err)

showTasks :: UTCTime -> State -> OnlyIdsOpt -> OnlyTagsOpt -> MoreOpt -> JsonOpt -> Query
showTasks now state onlyIdsOpt onlyTagsOpt moreOpt jsonOpt
  | onlyIdsOpt = ShowTasks now rtype (getVisibleTasks state) -- TODO
  | onlyTagsOpt = ShowTasks now rtype (getVisibleTasks state) -- TODO
  | otherwise = ShowTasks now rtype (getVisibleTasks state)
  where
    rtype = if jsonOpt then Json else Text

showTask :: UTCTime -> State -> Id -> JsonOpt -> Query
showTask now state id jsonOpt = case findById id (getTasks state) of
  Nothing -> Error rtype "info" "task not found"
  Just task -> ShowTask now rtype task
  where
    rtype = if jsonOpt then Json else Text

showWtime :: UTCTime -> State -> [Tag] -> FromOpt -> ToOpt -> MoreOpt -> JsonOpt -> Query
showWtime now state tags fromOpt toOpt moreOpt jsonOpt = ShowWtime now rtype wtimes
  where
    tasks = getTasks state
    wtimes = buildWtimePerDay now fromOpt toOpt tasks
    rtype = if jsonOpt then Json else Text

showStatus :: UTCTime -> State -> MoreOpt -> JsonOpt -> Query
showStatus now state moreOpt jsonOpt = ShowStatus now rtype $ findFstActiveTask (getTasks state)
  where
    rtype = if jsonOpt then Json else Text

showVersion :: JsonOpt -> Query
showVersion jsonOpt = ShowVersion rtype "1.0.0"
  where
    rtype = if jsonOpt then Json else Text

doUpgrade :: IO ()
doUpgrade = system "curl -sSL https://raw.githubusercontent.com/soywod/unfog.cli/master/bin/install.sh | sh" >> return ()
