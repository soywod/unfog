module Query.Handler where

import Arg.Options
import Arg.Parser
import Data.List
import Data.Time
import Event
import Response
import State
import Task

handleQuery :: Query -> IO ()
handleQuery (List onlyIds onlyTags more json) = do
  state <- rebuild <$> readEvents
  listTasks state onlyIds onlyTags more json

listTasks :: State -> OnlyIdsOpt -> OnlyTagsOpt -> MoreOpt -> JsonOpt -> IO ()
listTasks state onlyIds onlyTags more json
  | onlyIds = print . map getId . getTasks $ state
  | onlyTags = print . nub . concatMap getTags . getTasks $ state
  | otherwise = print . getTasks $ state
