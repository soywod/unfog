module Query where

import qualified Data.ByteString.Lazy.Char8    as BL
import           Control.Exception
import           Data.Maybe
import           Text.Read
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
  | Error String String
  deriving (Show)

handle :: Parsec.ArgTree -> IO ()
handle args = do
  evts <- readEvents
  let state = applyEvents evts
  let qry   = getQry args
  execute args state evts qry

getQry :: Parsec.ArgTree -> Query
getQry args = case Parsec._cmd args of
  "list"     -> ShowTasks args
  "worktime" -> ShowWtime args
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
      print $ map _tags $ fByTags $ filter _done $ _tasks state
      let fByDone = filterByDone $ "done" `elem` ctx
      let tasks   = mapWithWtime now . fByTags . fByDone $ _tasks state
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
      let tags  = Parsec._tags args
      let ids   = map _id $ filterByTags tags $ _tasks state
      let tasks = filterByIds ids $ mapWithWtime now $ _tasks state
      let wtime = getWtimePerDay now tasks
      let ctx = if null tags then "global" else "for [" ++ unwords tags ++ "]"
      printWtime rtype ("unfog: wtime " ++ ctx) wtime

    Query.Error command message -> printErr rtype $ command ++ ": " ++ message
