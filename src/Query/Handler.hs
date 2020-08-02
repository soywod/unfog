module Query.Handler where

import Arg.Options
import qualified Arg.Parser as Arg
import Data.List
import Data.Time
import Event
import Response
import State
import Task

data Query
  = ShowTasks UTCTime ResponseType [Task]
  | ShowTask UTCTime ResponseType Task
  | ShowWtime String
  | ShowStatus String
  | ShowVersion String
  | DoUpgrade
  | Error String String
  deriving (Show, Read)

handleQuery :: Arg.Query -> IO ()
handleQuery arg = do
  now <- getCurrentTime
  state <- rebuild <$> readEvents
  putStrLn $ execute $ mapArgToQuery now state arg

mapArgToQuery :: UTCTime -> State -> Arg.Query -> Query
mapArgToQuery now state (Arg.List onlyIdsOpt onlyTagsOpt moreOpt jsonOpt) = showTasks now state onlyIdsOpt onlyTagsOpt moreOpt jsonOpt
mapArgToQuery now state (Arg.Info id moreOpt jsonOpt) = showTask now state id jsonOpt
mapArgToQuery now state (Arg.Wtime tags fromOpt toOpt moreOpt jsonOpt) = showWtime now state tags fromOpt toOpt moreOpt jsonOpt
mapArgToQuery _ state (Arg.Status moreOpt jsonOpt) = showStatus state moreOpt jsonOpt
mapArgToQuery _ state (Arg.Version jsonOpt) = showVersion jsonOpt
mapArgToQuery _ state Arg.Upgrade = DoUpgrade

showTasks :: UTCTime -> State -> OnlyIdsOpt -> OnlyTagsOpt -> MoreOpt -> JsonOpt -> Query
showTasks now state onlyIdsOpt onlyTagsOpt moreOpt jsonOpt
  | onlyIdsOpt = ShowTasks now rtype (getTasks state)
  | onlyTagsOpt = ShowTasks now rtype (getTasks state)
  | otherwise = ShowTasks now rtype (getTasks state)
  where
    rtype = if jsonOpt then Json else Text

showTask :: UTCTime -> State -> Id -> JsonOpt -> Query
showTask now state id jsonOpt = case findById id (getTasks state) of
  Nothing -> Error "info" "task not found"
  Just task -> ShowTask now rtype task
  where
    rtype = if jsonOpt then Json else Text

showWtime :: UTCTime -> State -> [Tag] -> FromOpt -> ToOpt -> MoreOpt -> JsonOpt -> Query
showWtime now state tags fromOpt toOpt moreOpt jsonOpt = ShowWtime "wtime"

showStatus :: State -> MoreOpt -> JsonOpt -> Query
showStatus state moreOpt jsonOpt = ShowWtime "status"

showVersion :: JsonOpt -> Query
showVersion jsonOpt = ShowVersion "1.0.0"

execute :: Query -> String
execute (ShowTasks now rtype tasks) = response rtype $ TasksResponse now tasks
execute (ShowTask now rtype task) = response rtype $ TaskResponse now task
execute DoUpgrade = "UPGRADE"
execute (Error key err) = key ++ ": " ++ err
execute query = show query
