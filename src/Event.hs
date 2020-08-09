module Event where

import Control.Monad
import Data.Time (UTCTime)
import qualified File
import Task

data Event
  = TaskAdded UTCTime Id Desc Project Due
  | TaskEdited UTCTime Id Desc Project Due
  | TaskStarted UTCTime Id
  | TaskStopped UTCTime Id
  | TaskDid UTCTime Id
  | TaskUndid UTCTime Id
  | TaskDeleted UTCTime Id
  | TaskUndeleted UTCTime Id
  | ContextEdited Project
  deriving (Show, Read)

readFile :: IO [Event]
readFile = map read . lines <$> File.getContent "store"

writeFile :: [Event] -> IO ()
writeFile = mapM_ writeEvent
  where
    writeEvent evt = appendToStore evt =<< File.getPath "store"
    appendToStore evt store = appendFile store $ show evt ++ "\n"
