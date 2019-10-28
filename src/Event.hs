module Event where

import           Data.Time

import           Task

data Event = TaskAdded UTCTime Task | TaskEdited UTCTime Task deriving (Show, Read)
