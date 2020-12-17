module Unfog.Event.MigrationV0 (handleEvts) where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import System.Random
import Unfog.Event.Type (Event (..), readEvents, sortOnUTCTime)
import Unfog.Event.TypeV0 (EventV0)
import qualified Unfog.Event.TypeV0 as EventV0
import qualified Unfog.File as File
import Unfog.Task

handleEvts :: IO (Maybe [Event])
handleEvts = do
  maybeEvts <- foldr readEvents (Just []) . lines <$> (File.readFromName "store" <|> return "")
  uuids <- generateUUIDs maybeEvts
  return $ sortOnUTCTime . foldl1 (++) . Map.elems . buildMap uuids <$> maybeEvts

-- Utils

buildMap :: [String] -> [EventV0] -> Map Int [Event]
buildMap uuids = foldl buildMap' Map.empty
  where
    buildMap' m (EventV0.TaskCreated t ref _ _ desc tags due) = Map.insert ref [TaskAdded t (uuids !! Map.size m) desc (mapTagsToProj tags) due] m
    buildMap' m (EventV0.TaskUpdated t ref _ _ desc tags due) = Map.adjust (++ [TaskEdited t (getUUID ref m) desc (mapTagsToProj tags) due]) ref m
    buildMap' m (EventV0.TaskStarted t ref _) = Map.adjust (++ [TaskStarted t (getUUID ref m)]) ref m
    buildMap' m (EventV0.TaskStopped t ref _) = Map.adjust (++ [TaskStopped t (getUUID ref m)]) ref m
    buildMap' m (EventV0.TaskMarkedAsDone t ref _) = Map.adjust (++ [TaskDid t (getUUID ref m)]) ref m
    buildMap' m (EventV0.TaskUnmarkedAsDone t ref _) = Map.adjust (++ [TaskUndid t (getUUID ref m)]) ref m
    buildMap' m (EventV0.TaskDeleted t ref _) = Map.adjust (++ [TaskDeleted t (getUUID ref m)]) ref m
    buildMap' m (EventV0.ContextSet t tags) = Map.adjust (++ [ContextEdited t (mapTagsToProj tags)]) 0 m
    getUUID ref m = case Map.lookup ref m of
      Nothing -> error $ "missing ref " ++ show ref ++ " in event map"
      Just evts -> case head evts of
        TaskAdded _ uuid _ _ _ -> uuid
        _ -> error $ "missing create event for " ++ show ref

mapTagsToProj :: [String] -> Project
mapTagsToProj tags
  | null tags = Nothing
  | otherwise = Just $ intercalate "-" tags

generateUUIDs :: Maybe [EventV0] -> IO [String]
generateUUIDs evts = replicateM evtsLen generateUUID
  where
    evtsLen = maybe 0 length evts
    generateUUID = show <$> (randomIO :: IO UUID)
