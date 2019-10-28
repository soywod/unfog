module Task where

data Task = Task { _id :: Int
                 , _desc :: String
                 } deriving (Show, Read)
