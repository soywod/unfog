module Query where

import qualified Data.ByteString.Lazy.Char8    as BL
import           Control.Exception
import           Data.Maybe
import           Text.Read
import           Data.Time
import           Data.Time.Clock
import           Data.Duration
import           Data.Fixed
import           Data.List
import           Text.PrettyPrint.Boxes
import           Data.Aeson

import           Store
import           State
import           Task
import           Utils
import           Event
import           Response
import qualified Parsec

data Query
  = ShowTasks Parsec.ArgTree
  | ShowTask Parsec.ArgTree
  | ShowWtime Parsec.ArgTree
  | ShowHelp
  | ShowVersion
  | Error String String
  deriving (Show)

handle :: Parsec.ArgTree -> IO ()
handle args = do
  evts <- readEvents
  now  <- getCurrentTime
  let state = applyEvents now evts
  let qry   = getQuery args
  execute args state evts qry

getQuery :: Parsec.ArgTree -> Query
getQuery args = case Parsec._cmd args of
  "list"     -> ShowTasks args
  "worktime" -> ShowWtime args
  "help"     -> ShowHelp
  "version"  -> ShowVersion
  "show"     -> case Parsec._id args of
    0  -> Query.Error "show" "invalid arguments"
    id -> ShowTask args

execute :: Parsec.ArgTree -> State -> [Event] -> Query -> IO ()
execute args state events query = do
  let rtype = if Parsec._json (Parsec._opts args) then JSON else Text
  case query of
    ShowTasks args -> do
      now <- getCurrentTime
      let ctx     = _ctx state
      let fByTags = filterByTags ctx
      let fByDone = filterByDone $ "done" `elem` ctx
      let tasks = mapWithWtime now . fByTags . fByDone $ _tasks state
      case rtype of
        JSON -> printTasks JSON tasks
        Text -> do
          putStrLn $ "unfog: list" ++ if null ctx
            then ""
            else " [" ++ unwords ctx ++ "]"
          printTasks Text tasks

    ShowTask args -> do
      now <- getCurrentTime
      let ctx       = _ctx state
      let fByTags   = filterByTags $ _ctx state
      let fByDone   = filterByDone $ "done" `elem` ctx
      let fByNumber = findById $ Parsec._id args
      let maybeTask = fByNumber . fByTags . fByDone $ _tasks state
      case maybeTask of
        Nothing   -> printErr rtype "show: task not found"
        Just task -> printTask rtype $ task { _wtime = getTotalWtime now task }

    ShowWtime args -> do
      now <- getCurrentTime
      let tags = Parsec._tags args `union` _ctx state
      let min  = Parsec.parseMinDate now args
      print min
      let ids   = map _id $ filterByTags tags $ _tasks state
      let tasks = filterByIds ids $ _tasks state
      let wtime = getWtimePerDay now min tasks
      let ctx = if null tags then "global" else "for [" ++ unwords tags ++ "]"
      printWtime rtype ("unfog: wtime " ++ ctx) wtime

    ShowHelp -> do
      putStrLn "Usage: unfog <cmd> [args...] [opts...]"
      putStrLn "unfog <create|add> <desc> [+tag...] [--json]"
      putStrLn "unfog <update|edit> <id> [desc] [+tag...] [-tag...] [--json]"
      putStrLn "unfog <replace|set> <id> <desc> [+tag...] [--json]"
      putStrLn "unfog <start|stop|toggle> <id> [--json]"
      putStrLn "unfog <done|delete|remove> <id> [--json]"
      putStrLn "unfog <context|ctx> [+tag...] [--json]"
      putStrLn "unfog <list> [--json]"
      putStrLn "unfog <show> <id> [--json]"
      putStrLn "unfog <worktime|wtime> [+tag...] [--json]"
      putStrLn "unfog <help|h>"
      putStrLn "unfog <version|v>"

    ShowVersion                 -> printVersion rtype "0.2.1"

    Query.Error command message -> printErr rtype $ command ++ ": " ++ message

