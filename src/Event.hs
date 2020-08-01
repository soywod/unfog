module Event where

import Data.Time (UTCTime)
import Task
import File

data Event
  = TaskCreated UTCTime Id Desc [Tag]
  | TaskUpdated UTCTime Id Desc [Tag]
  | TaskStarted UTCTime Id
  | TaskStopped UTCTime Id
  | TaskMarkedAsDone UTCTime Id
  | TaskUnmarkedAsDone UTCTime Id
  | TaskDeleted UTCTime Id
  | ContextUpdated UTCTime [Tag]
  deriving (Show, Read)

readEvents :: IO [Event]
readEvents = map read . lines <$> getFileContent "store"

writeEvents :: [Event] -> IO ()
writeEvents = foldr write' (return ())
  where
    write' evt _ = getFilePath "store" >>= appendToStore evt
    appendToStore evt = flip appendFile $ show evt ++ "\n"
