module Event where

import           Data.Time

import           Task

data Event = TaskAdded UTCTime Task deriving (Show, Read)
