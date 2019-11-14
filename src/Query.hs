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

import qualified Store
import           State
import           Task
import           Utils
import           Event
import           Response

data Query
  = ShowTasks [String]
  | ShowTask Id [String]
  | ShowWorktime [String]
  | Error String String
  deriving (Show)

handle :: ResponseType -> [String] -> IO ()
handle rtype args = do
  events <- Store.readAll
  let state = State.applyAll events
  let query = parseArgs args
  execute rtype state events query

parseArgs :: [String] -> Query
parseArgs args = case args of
  ("list"          : args) -> ShowTasks args
  ("worktime"      : args) -> ShowWorktime args
  ("show" : number : args) -> case readMaybe number of
    Nothing     -> Query.Error "show" "task not found"
    Just number -> ShowTask number args

execute :: ResponseType -> State -> [Event] -> Query -> IO ()
execute rtype state events query = case query of
  ShowTasks args -> do
    now <- getCurrentTime
    let fByTags = filterByTags $ _context state
    let fByDone = filterByDone $ _showDone state
    let tasks   = mapWithWorktime now . fByTags . fByDone $ _tasks state
    case rtype of
      JSON -> printTasks JSON tasks
      Text -> do
        let ctx    = [ "done" | _showDone state ] ++ _context state
        let ctxStr = if null ctx then "" else " [" ++ unwords ctx ++ "]"
        putStrLn $ "unfog: list" ++ ctxStr
        printTasks Text tasks

  ShowTask id args -> do
    now <- getCurrentTime
    let fByTags   = filterByTags $ _context state
    let fByDone   = filterByDone $ _showDone state
    let fByNumber = findById id
    let maybeTask = fByNumber . fByTags . fByDone $ _tasks state
    case maybeTask of
      Nothing   -> printErr rtype "show: task not found"
      Just task -> printTask rtype $ task { _wtime = getWorktime now task }

  ShowWorktime args -> do
    now <- getCurrentTime
    let tags  = filter startsByPlus args
    let ids   = map _id $ filterByTags args $ _tasks state
    let tasks = filterByIds ids $ mapWithWorktime now $ _tasks state
    let total = sum $ map _wtime tasks
    printMsg rtype
      $  "worktime "
      ++ (if null args then "global" else "for [" ++ unwords tags ++ "]")
      ++ ": "
      ++ humanReadableDuration total

  Query.Error command message -> printErr rtype $ command ++ ": " ++ message
