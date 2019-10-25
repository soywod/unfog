module Dispatch
  ( dispatch
  )
where

import           Prelude                 hiding ( id )
import           Control.Exception
import           Data.Maybe
import           System.Environment
import           System.IO
import           System.IO.Error
import           System.Directory

data Command = AddTask Task deriving (Show, Read)
data Event = TaskAdded Task deriving (Show, Read)

data Task = Task { id :: Int
                 , desc :: String
                 } deriving (Show, Read)

data State = State { tasks :: [Task]
                   } deriving (Show, Read)

dispatch :: [String] -> IO ()
dispatch args = do
  currState <- readState
  command   <- return $ getCommandFromArgs args
  event     <- return $ getEventFromCommand currState command
  nextState <- return $ buildStateFromEvent currState event
  writeEvent event
  writeState nextState

getCommandFromArgs :: [String] -> Command
getCommandFromArgs ("add" : args) =
  AddTask Task { id = 0, desc = unwords args }

getEventFromCommand :: State -> Command -> Event
getEventFromCommand state (AddTask task) = TaskAdded task

buildStateFromEvent :: State -> Event -> State
buildStateFromEvent state event = case event of
  TaskAdded task -> state { tasks = tasks state ++ [task] }

readState :: IO State
readState = tryReadStateFileContent >>= catchDoesNotExist
 where
  catchDoesNotExist result = case result of
    Right state               -> return $ read state
    Left  isDoesNotExistError -> return State { tasks = [] }

tryReadStateFileContent :: IO (Either IOError String)
tryReadStateFileContent = try $ getFilePath "state.txt" >>= readFile

getFilePath :: String -> IO String
getFilePath file = (++ "/" ++ file) <$> getConfigDirPath

getConfigDirPath :: IO String
getConfigDirPath = lookupEnv "XDG_CONFIG_HOME" >>= withDefault
 where
  withDefault maybePath = case maybePath of
    Just path -> (++ "/unfog") <$> return path
    Nothing   -> (++ "/.config/unfog") . fromMaybe "/tmp" <$> lookupEnv "HOME"

writeState :: State -> IO ()
writeState state = do
  stateFilePath        <- getFilePath "state.txt"
  (tmpPath, tmpHandle) <- openTempFile "/tmp" "state.txt"
  hPutStr tmpHandle $ show state
  hClose tmpHandle
  removeFile stateFilePath `catch` handleRemoveFile
  copyFile tmpPath stateFilePath
  removeFile tmpPath
 where
  handleRemoveFile err | isDoesNotExistError err = return ()
                       | otherwise               = throwIO err

writeEvent :: Event -> IO ()
writeEvent event =
  getFilePath "events.txt" >>= flip appendFile (show event ++ "\n")
