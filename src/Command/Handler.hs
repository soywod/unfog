module Command.Handler where

import qualified Arg.Parser as Arg
import Data.List
import Data.Time
import Data.UUID.V4
import Event
import State
import Task
import Prelude hiding (id)

data Command
  = CreateTask UTCTime Id Desc [Tag]
  | UpdateTask UTCTime Id Desc [Tag]
  | StartTask UTCTime Id
  | StopTask UTCTime Id
  | MarkAsDoneTask UTCTime Id
  | UnmarkAsDoneTask UTCTime Id
  | DeleteTask UTCTime Id
  | UpdateContext UTCTime [Tag]
  | Error String String
  deriving (Show, Read)

handleCommand :: Arg.Command -> IO ()
handleCommand command = case command of
  Arg.Add desc -> do
    state <- rebuild <$> readEvents
    now <- getCurrentTime
    id <- show <$> nextRandom
    let cmd = createTask state now id desc
    let evts = execute state cmd
    writeEvents evts
  Arg.Edit id desc -> do
    state <- rebuild <$> readEvents
    now <- getCurrentTime
    let cmd = updateTask state now id desc
    let evts = execute state cmd
    writeEvents evts

execute :: State -> Command -> [Event]
execute state command = case command of
  CreateTask now id desc tags -> [TaskCreated now id desc tags]
  UpdateTask now id desc tags -> [TaskUpdated now id desc tags]
  Error _ _ -> []

createTask :: State -> UTCTime -> Id -> Desc -> Command
createTask state now id desc = CreateTask now id desc (getContext state)

updateTask :: State -> UTCTime -> Id -> Desc -> Command
updateTask state now id desc = case maybeTask of
  Nothing -> Error "update" "task not found"
  Just task -> validate task
  where
    maybeTask = findById id (getTasks state)
    validate task
      | getDone task = Error "update" "task already done"
      | otherwise = UpdateTask now (getId task) desc (getTags task)
