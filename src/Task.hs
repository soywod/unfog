module Task where

data Task = Task { _id :: Int
                 , _desc :: String
                 , _tags :: [String]
                 } deriving (Show, Read)
