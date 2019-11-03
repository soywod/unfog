module Task where

import           Control.Exception
import           Data.List
import           Text.PrettyPrint.Boxes

import           Utils

type Id = Int
type Number = Int
type Desc = String
type Tag = String

data Task = Task { _id :: Id
                 , _number :: Number
                 , _desc :: Desc
                 , _tags :: [Tag]
                 , _active :: Bool
                 , _done :: Bool
                 } deriving (Show, Read, Eq)

emptyTask :: Task
emptyTask = Task { _id     = 0
                 , _number = 0
                 , _desc   = ""
                 , _tags   = []
                 , _active = False
                 , _done   = False
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

filterByTags :: [Tag] -> [Task] -> [Task]
filterByTags tags tasks = filteredTasks
 where
  filteredTasks = if null tags then tasks else filter byTags tasks
  byTags        = not . null . intersect tags . _tags

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
    , unwords $ _tags task
    , if _active task then "✔" else ""
    ]
