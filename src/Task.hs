module Task where

data Task = Task { id :: Int
                 , desc :: String
                 } deriving (Show, Read)
