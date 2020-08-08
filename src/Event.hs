module Event where

import Control.Monad
import Data.Time (UTCTime)
import File
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

readEvents :: IO [Event]
readEvents = map read . lines <$> getFileContent "store"

writeEvents :: [Event] -> IO ()
writeEvents = mapM_ writeEvent
  where
    writeEvent evt = appendToStore evt =<< getFilePath "store"
    appendToStore evt store = appendFile store $ show evt ++ "\n"
