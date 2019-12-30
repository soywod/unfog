module Query where

import qualified Data.ByteString.Lazy.Char8    as BL
import           Control.Exception
import           Data.Aeson              hiding ( Error )
import           Data.Duration
import           Data.Fixed
import           Data.List
import           Data.Maybe
import           Data.Time
import           Text.PrettyPrint.Boxes
import           Text.Read

import           Event
import           Response
import           State
import           Store
import           Task
import           Utils
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
  "show"     -> case Parsec._ids args of
    []     -> Error "show" "invalid arguments"
    id : _ -> ShowTask args

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
      let id        = head $ Parsec._ids args
      let fByTags   = filterByTags $ _ctx state
      let fByDone   = filterByDone $ "done" `elem` ctx
      let fByNumber = findById id
      let maybeTask = fByNumber . fByTags . fByDone $ _tasks state
      case maybeTask of
        Nothing   -> printErr rtype $ "show: task [" ++ show id ++ "] not found"
        Just task -> printTask rtype $ task { _wtime = getTotalWtime now task }

    ShowWtime args -> do
      now <- getCurrentTime
      let tags  = Parsec._tags args `union` _ctx state
      let min   = Parsec.parseMinDate now args
      let max   = Parsec.parseMaxDate now args
      let refs  = map _ref $ filterByTags tags $ _tasks state
      let tasks = filterByRefs refs $ _tasks state
      let wtime = getWtimePerDay now min max tasks
      let ctx = if null tags then "global" else "for [" ++ unwords tags ++ "]"
      printWtime rtype ("unfog: wtime " ++ ctx) wtime

    ShowHelp -> do
      putStrLn "Usage: unfog cmd (args...)"
      putStrLn "unfog create desc (+tags) (:due:time) (--json)"
      putStrLn "unfog update ids (desc) (+tags) (-tags) (:due:time) (--json)"
      putStrLn "replace ids desc (+tags) (:due:time) (--json)"
      putStrLn "start ids (--json)"
      putStrLn "stop ids (--json)"
      putStrLn "toggle ids (--json)"
      putStrLn "done ids (--json)"
      putStrLn "delete ids (--json)"
      putStrLn "remove ids (--json)"
      putStrLn "context (+tags) (--json)"
      putStrLn "list (--json)"
      putStrLn "show id (--json)"
      putStrLn "worktime (+tags) ([min:time) (]max:time) (--json)"
      putStrLn "help"
      putStrLn "version"

    ShowVersion           -> printVersion rtype "0.3.2"

    Error command message -> printErr rtype $ command ++ ": " ++ message
