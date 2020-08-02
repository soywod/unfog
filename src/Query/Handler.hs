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
  = PrintTasks String
  | PrintTask String
  | PrintWtime String
  | PrintStatus String
  | PrintVersion String
  | DoUpgrade
  | Error String String
  deriving (Show, Read)

handleQuery :: Arg.Query -> IO ()
handleQuery query = case query of
  Arg.List onlyIdsOpt onlyTagsOpt moreOpt jsonOpt -> do
    state <- rebuild <$> readEvents
    let query = printTasks state onlyIdsOpt onlyTagsOpt moreOpt jsonOpt
    execute query
  Arg.Info id moreOpt jsonOpt -> do
    state <- rebuild <$> readEvents
    let query = printTask state id
    execute query
  Arg.Wtime tags fromOpt toOpt moreOpt jsonOpt -> do
    state <- rebuild <$> readEvents
    let query = printWtime state tags fromOpt toOpt moreOpt jsonOpt
    execute query
  Arg.Status moreOpt jsonOpt -> do
    state <- rebuild <$> readEvents
    let query = printStatus state moreOpt jsonOpt
    execute query
  Arg.Version jsonOpt -> do
    state <- rebuild <$> readEvents
    let query = printVersion jsonOpt
    execute query
  Arg.Upgrade -> do
    state <- rebuild <$> readEvents
    execute DoUpgrade

execute :: Query -> IO ()
execute DoUpgrade = putStrLn "UPGRADE"
execute (Error key err) = putStrLn $ key ++ ": " ++ err
execute query = print query

printTasks :: State -> OnlyIdsOpt -> OnlyTagsOpt -> MoreOpt -> JsonOpt -> Query
printTasks state onlyIds onlyTags more json
  | onlyIds = PrintTasks . show . map getId . getTasks $ state
  | onlyTags = PrintTasks . show . nub . concatMap getTags . getTasks $ state
  | otherwise = PrintTasks . show . getTasks $ state

printTask :: State -> Id -> Query
printTask state id = case findById id (getTasks state) of
  Nothing -> Error "info" "task not found"
  Just task -> PrintTask . show $ task

printWtime :: State -> [Tag] -> FromOpt -> ToOpt -> MoreOpt -> JsonOpt -> Query
printWtime state tags fromOpt toOpt moreOpt jsonOpt = PrintWtime "wtime"

printStatus :: State -> MoreOpt -> JsonOpt -> Query
printStatus state moreOpt jsonOpt = PrintWtime "status"

printVersion :: JsonOpt -> Query
printVersion jsonOpt = PrintVersion "1.0.0"
