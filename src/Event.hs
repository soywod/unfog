module Event where

import           Task

newtype Event = TaskAdded Task deriving (Show, Read)
