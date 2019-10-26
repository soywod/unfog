module Dispatch
  ( handleArgs
  )
where

import           Prelude                 hiding ( id )
import           Control.Exception
import           Data.Maybe
import           System.Environment
import           System.IO
import           System.IO.Error
import           System.Directory

data Query = ShowTasks deriving (Show)
newtype Command = AddTask Task deriving (Show, Read)
newtype Event = TaskAdded Task deriving (Show, Read)

data Task = Task { id :: Int
                 , desc :: String
                 } deriving (Show, Read)

newtype State = State { tasks :: [Task]
                      } deriving (Show, Read)

emptyState :: State
emptyState = State { tasks = [] }

handleArgs :: [String] -> IO ()
handleArgs ("add"  : args) = handleCommand $ "add" : args
handleArgs ("list" : args) = handleQuery $ "list" : args

handleCommand :: [String] -> IO ()
handleCommand args = do
  events <- readEvents
  let state   = foldl buildStateFromEvent emptyState events
  let command = getCommandFromArgs args
  let event   = getEventFromCommand state command
  writeEvent event

handleQuery :: [String] -> IO ()
handleQuery args = do
  events <- readEvents
  let state = foldl buildStateFromEvent emptyState events
  let query = getQueryFromArgs args
  processQuery state query

getCommandFromArgs :: [String] -> Command
getCommandFromArgs ("add" : args) =
  AddTask Task { id = 0, desc = unwords args }

getQueryFromArgs :: [String] -> Query
getQueryFromArgs ("list" : args) = ShowTasks

processQuery :: State -> Query -> IO ()
processQuery state query = case query of
  ShowTasks -> print $ tasks state

getEventFromCommand :: State -> Command -> Event
getEventFromCommand state (AddTask task) = TaskAdded task

buildStateFromEvent :: State -> Event -> State
buildStateFromEvent state event = case event of
  TaskAdded task -> state { tasks = tasks state ++ [task] }

getFilePath :: String -> IO String
getFilePath file = (++ "/" ++ file) <$> getConfigDirPath

getConfigDirPath :: IO String
getConfigDirPath = lookupEnv "XDG_CONFIG_HOME" >>= withDefault
 where
  withDefault maybePath = case maybePath of
    Just path -> (++ "/unfog") <$> return path
    Nothing   -> (++ "/.config/unfog") . fromMaybe "/tmp" <$> lookupEnv "HOME"

readEvents :: IO [Event]
readEvents = do
  let tmpStorePath = "/tmp/unfog-store"
  storePath <- getFilePath "store"
  copyFile storePath tmpStorePath `catch` catchCopyErrors
  storeContent <- readFile tmpStorePath `catch` catchReadErrors
  removeFile tmpStorePath `catch` catchRemoveErrors
  return $ map read $ lines storeContent
 where
  catchCopyErrors err | isDoesNotExistError err = return ()
                      | otherwise               = throwIO err
  catchReadErrors err | isDoesNotExistError err = return ""
                      | otherwise               = throwIO err
  catchRemoveErrors err | isDoesNotExistError err = return ()
                        | otherwise               = throwIO err

writeEvent :: Event -> IO ()
writeEvent event = getFilePath "store" >>= flip appendFile (show event ++ "\n")
