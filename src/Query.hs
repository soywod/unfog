module Query where

import Control.Exception
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Time
import Event
import qualified Parsec
import Response
import State
import Store
import System.Process (system)
import Task
import Text.Read

data Query
  = List Parsec.Arg
  | Info Parsec.Arg
  | Wtime Parsec.Arg
  | Status
  | Upgrade
  | Version
  | Help
  | Error String String
  deriving (Show)

handle :: Parsec.Arg -> IO ()
handle args = do
  evts <- readEvents
  now <- getCurrentTime
  let state = applyEvents now evts
  let qry = getQuery args
  execute args state qry

getQuery :: Parsec.Arg -> Query
getQuery args = case Parsec._cmd args of
  "list" -> List args
  "info" -> case Parsec._ids args of
    [] -> Error "show" "invalid arguments"
    id : _ -> Info args
  "worktime" -> Wtime args
  "status" -> Status
  "upgrade" -> Upgrade
  "version" -> Version
  "help" -> Help

execute :: Parsec.Arg -> State -> Query -> IO ()
execute args state query = do
  let rtype = if Parsec._json (Parsec._opts args) then JSON else Text
  let more = Parsec._more (Parsec._opts args)
  case query of
    List args -> listTasks args state query rtype
    Info args -> do
      now <- getCurrentTime
      let ctx = _ctx state
      let id = head $ Parsec._ids args
      let fByTags = filterByTags $ _ctx state
      let fByDone = filterByDone $ "done" `elem` ctx
      let fByNumber = findById id
      let maybeTask = fByNumber . fByTags . fByDone $ _tasks state
      case maybeTask of
        Nothing -> printErr rtype $ "show: task [" ++ show id ++ "] not found"
        Just task -> printTask rtype task {_wtime = getTotalWtime now task}
    Wtime args -> do
      now <- getCurrentTime
      let tags = Parsec._tags args `union` _ctx state
      let min = Parsec.parseMinDate now args
      let max = Parsec.parseMaxDate now args
      let refs = map _ref $ filterByTags tags $ _tasks state
      let tasks = filterByRefs refs $ _tasks state
      let wtime = getWtimePerDay now min max tasks
      let ctx = if null tags then "global" else "for [" ++ unwords tags ++ "]"
      printWtime rtype more ("unfog: wtime " ++ ctx) wtime
    Status -> do
      now <- getCurrentTime
      case filter ((> 0) . _active) $ _tasks state of
        [] -> printEmptyStatus rtype
        task : _ -> printStatus rtype task
    Upgrade ->
      system
        "curl -sSL https://raw.githubusercontent.com/soywod/unfog.cli/master/bin/install.sh | sh"
        >> return ()
    Version -> printVersion rtype "0.4.4"
    Help -> do
      putStrLn "Usage: unfog cmd (args)"
      putStrLn ""
      putStrLn "Create a task:"
      putStrLn "create|add desc (+tags) (:due:time) (--json)"
      putStrLn ""
      putStrLn "Update a task (tags are kept):"
      putStrLn "update|edit ids (desc) (+tags) (-tags) (:due:time) (--json)"
      putStrLn ""
      putStrLn "Replace a task:"
      putStrLn "replace|set ids desc (+tags) (:due:time) (--json)"
      putStrLn ""
      putStrLn "Start/stop/toggle a task:"
      putStrLn "start ids (--json)"
      putStrLn "stop ids (--json)"
      putStrLn "toggle ids (--json)"
      putStrLn ""
      putStrLn "Mark as done a task:"
      putStrLn "done ids (--json)"
      putStrLn ""
      putStrLn "Delete a task:"
      putStrLn "delete ids (--json)"
      putStrLn ""
      putStrLn "If not done, mark as done, else delete a task:"
      putStrLn "remove ids (--json)"
      putStrLn ""
      putStrLn "Set a context:"
      putStrLn "context|ctx (+tags) (--json)"
      putStrLn ""
      putStrLn "List tasks:"
      putStrLn "list (--json)"
      putStrLn ""
      putStrLn "Show a task:"
      putStrLn "show id (--json)"
      putStrLn ""
      putStrLn "Display total worktime by context:"
      putStrLn "worktime|wtime (+tags) ([min:time) (]max:time) (--json)"
    Error command message -> printErr rtype $ command ++ ": " ++ message

listTasks :: Parsec.Arg -> State -> Query -> ResponseType -> IO ()
listTasks args state query rtype
  | Parsec._onlyIds (Parsec._opts args) =
    printTasksId rtype . map Task._id . fByTags . fByDone $ allTasks
  | Parsec._onlyTags (Parsec._opts args) =
    printTasksTags rtype . nub . concatMap Task._tags $ allTasks
  | otherwise =
    do
      now <- getCurrentTime
      let ctx = _ctx state
      let fByTags = filterByTags ctx
      let fByDone = filterByDone $ "done" `elem` ctx
      let tasks = mapWithWtime now . fByTags . fByDone $ _tasks state
      let ctxStr = if null ctx then "" else " [" ++ unwords ctx ++ "]"
      let onlyIds = Parsec._onlyIds (Parsec._opts args)
      let onlyTags = Parsec._onlyTags (Parsec._opts args)
      printTasks rtype ("unfog: list" ++ ctxStr) tasks
  where
    allTasks = _tasks state
    fByTags = filterByTags . _ctx $ state
    fByDone = filterByDone . elem "done" . _ctx $ state
