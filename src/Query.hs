module Query where

import qualified Data.ByteString.Lazy.Char8    as BL
import           Control.Exception
import           Data.Aeson              hiding ( Error )
import           Data.Duration
import           Data.Fixed
import           Data.List
import           Data.Maybe
import           Data.Time
import           System.Process                 ( system )
import           Text.Read

import           Event
import           Response
import           State
import           Store
import           Task
import qualified Parsec

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
  now  <- getCurrentTime
  let state = applyEvents now evts
  let qry   = getQuery args
  execute args state evts qry

getQuery :: Parsec.Arg -> Query
getQuery args = case Parsec._cmd args of
  "list" -> List args
  "info" -> case Parsec._ids args of
    []     -> Error "show" "invalid arguments"
    id : _ -> Info args
  "worktime" -> Wtime args
  "status"   -> Status
  "upgrade"  -> Upgrade
  "version"  -> Version
  "help"     -> Help

execute :: Parsec.Arg -> State -> [Event] -> Query -> IO ()
execute args state events query = do
  let rtype = if Parsec._json (Parsec._opts args) then JSON else Text
  let more  = Parsec._more (Parsec._opts args)
  case query of
    List args -> do
      now <- getCurrentTime
      let ctx     = _ctx state
      let fByTags = filterByTags ctx
      let fByDone = filterByDone $ "done" `elem` ctx
      let tasks = mapWithWtime now . fByTags . fByDone $ _tasks state
      let ctxStr = if null ctx then "" else " [" ++ unwords ctx ++ "]"
      printTasks rtype ("unfog: list" ++ ctxStr) tasks

    Info args -> do
      now <- getCurrentTime
      let ctx       = _ctx state
      let id        = head $ Parsec._ids args
      let fByTags   = filterByTags $ _ctx state
      let fByDone   = filterByDone $ "done" `elem` ctx
      let fByNumber = findById id
      let maybeTask = fByNumber . fByTags . fByDone $ _tasks state
      case maybeTask of
        Nothing   -> printErr rtype $ "show: task [" ++ show id ++ "] not found"
        Just task -> printTask rtype task { _wtime = getTotalWtime now task }

    Wtime args -> do
      now <- getCurrentTime
      let tags  = Parsec._tags args `union` _ctx state
      let min   = Parsec.parseMinDate now args
      let max   = Parsec.parseMaxDate now args
      let refs  = map _ref $ filterByTags tags $ _tasks state
      let tasks = filterByRefs refs $ _tasks state
      let wtime = getWtimePerDay now min max tasks
      let ctx = if null tags then "global" else "for [" ++ unwords tags ++ "]"
      printWtime rtype more ("unfog: wtime " ++ ctx) wtime

    Status -> do
      now <- getCurrentTime
      case filter ((> 0) . _active) $ _tasks state of
        []       -> printEmptyStatus rtype
        task : _ -> printStatus rtype task

    Upgrade ->
      system
          "curl -sSL https://raw.githubusercontent.com/unfog-io/unfog-cli/master/install.sh | sh"
        >> return ()

    Version -> printVersion rtype "0.4.0"

    Help    -> do
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
