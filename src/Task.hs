{-# LANGUAGE OverloadedStrings #-}
module Task where

import qualified Data.ByteString.Lazy.Char8    as BL

import           Control.Exception
import           Data.Aeson
import           Data.Duration
import           Data.Fixed
import           Data.List
import           Data.Time
import           Text.PrettyPrint.Boxes

import           Utils

type Id = Int
type Number = Int
type Desc = String
type Tag = String

data Task =
  Task { _id :: Id
       , _number :: Number
       , _desc :: Desc
       , _tags :: [Tag]
       , _active :: Bool
       , _done :: Bool
       , _worktime :: Micro
       , _starts :: [UTCTime]
       , _stops :: [UTCTime]
       } deriving (Show, Read, Eq)

instance ToJSON Task where
  toJSON (Task id number desc tags active done worktime _ _) = object
    [ "id" .= number
    , "desc" .= desc
    , "tags" .= map tail tags
    , "active" .= if active then 1 else 0 :: Int
    , "done" .= if done then 1 else 0 :: Int
    , "worktime" .= object
      [ "approx" .= approximativeDuration worktime
      , "human" .= humanReadableDuration worktime
      , "micro" .= worktime
      ]
    ]

emptyTask :: Task
emptyTask = Task { _id       = 0
                 , _number   = 0
                 , _desc     = ""
                 , _tags     = []
                 , _active   = False
                 , _done     = False
                 , _worktime = 0
                 , _starts   = []
                 , _stops    = []
                 }

generateNumber :: [Task] -> Number
generateNumber tasks = generateNumber' (sort $ map _number tasks) [1 ..]
 where
  generateNumber' [] []           = 1
  generateNumber' [] (nextId : _) = nextId
  generateNumber' (currId : currIds) (nextId : nextIds)
    | currId == nextId = generateNumber' currIds nextIds
    | otherwise        = nextId

findById :: Id -> [Task] -> Maybe Task
findById id = find $ (==) id . _id

findByNumber :: Number -> [Task] -> Maybe Task
findByNumber number = find $ (==) number . _number

filterByDone :: Bool -> [Task] -> [Task]
filterByDone showDone tasks = filteredTasks
 where
  filteredTasks = filter byDone tasks
  byDone        = if showDone then _done else not . _done

filterByIds :: [Id] -> [Task] -> [Task]
filterByIds ids = filter (flip elem ids . _id)

filterByTags :: [Tag] -> [Task] -> [Task]
filterByTags tags tasks = filteredTasks
 where
  filteredTasks = if null tags then tasks else filter byTags tasks
  byTags        = not . null . intersect tags . _tags


mapWithWorktime :: UTCTime -> [Task] -> [Task]
mapWithWorktime now = map withWorktime
  where withWorktime task = task { _worktime = getWorktime now task }

getWorktime :: UTCTime -> Task -> Micro
getWorktime now task = worktime
 where
  starts   = _starts task
  stops    = _stops task ++ [ now | _active task ]
  worktime = realToFrac $ sum $ zipWith diffUTCTime stops starts

prettyPrint :: [Task] -> IO ()
prettyPrint tasks =
  putStrLn
    $ render
    $ table
    $ ["ID", "DESC", "TAGS", "ACTIVE"]
    : map prettyPrint' tasks
 where
  prettyPrint' task =
    [ show $ _number task
    , _desc task
    , unwords $ map tail $ _tags task
    , if _active task then "✔" else ""
    ]
