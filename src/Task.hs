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
type Reference = Int
type Position = Int
type Description = String
type Tag = String
type Due = Maybe UTCTime
type Active = Bool
type Done = Bool
type Worktime = Micro

data Task =
  Task { _id :: Id
       , _ref :: Reference
       , _pos :: Position
       , _desc :: Description
       , _tags :: [Tag]
       , _due :: Maybe Due
       , _active :: Bool
       , _done :: Bool
       , _wtime :: Worktime
       , _starts :: [UTCTime]
       , _stops :: [UTCTime]
       } deriving (Show, Read, Eq)

instance ToJSON Task where
  toJSON (Task id ref pos desc tags due active done wtime _ _) = object
    [ "id" .= id
    , "ref" .= ref
    , "pos" .= pos
    , "desc" .= desc
    , "tags" .= map tail tags
    , "active" .= if active then 1 else 0 :: Int
    , "done" .= if done then 1 else 0 :: Int
    , "wtime" .= object
      [ "approx" .= approximativeDuration wtime
      , "human" .= humanReadableDuration wtime
      , "micro" .= wtime
      ]
    ]

emptyTask :: Task
emptyTask = Task { _id     = 0
                 , _ref    = 0
                 , _pos    = -1
                 , _desc   = ""
                 , _tags   = []
                 , _due    = Nothing
                 , _active = False
                 , _done   = False
                 , _wtime  = 0
                 , _starts = []
                 , _stops  = []
                 }

generateId :: [Task] -> Id
generateId tasks = generateId' (sort $ map _id tasks) [1 ..]
 where
  generateId' [] []           = 1
  generateId' [] (nextId : _) = nextId
  generateId' (currId : currIds) (nextId : nextIds)
    | currId == nextId = generateId' currIds nextIds
    | otherwise        = nextId

findById :: Id -> [Task] -> Maybe Task
findById id = find $ (==) id . _id

findByRef :: Reference -> [Task] -> Maybe Task
findByRef ref = find $ (==) ref . _ref

filterByDone :: Done -> [Task] -> [Task]
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
  where withWorktime task = task { _wtime = getWorktime now task }

getWorktime :: UTCTime -> Task -> Worktime
getWorktime now task = realToFrac $ sum $ zipWith diffUTCTime stops starts
 where
  starts = _starts task
  stops  = _stops task ++ [ now | _active task ]

prettyPrint :: [Task] -> IO ()
prettyPrint tasks =
  putStrLn
    $ render
    $ table
    $ ["ID", "DESC", "TAGS", "ACTIVE"]
    : map prettyPrint' tasks
 where
  prettyPrint' task =
    [ show $ _id task
    , _desc task
    , unwords $ map tail $ _tags task
    , if _active task then "✔" else ""
    ]
