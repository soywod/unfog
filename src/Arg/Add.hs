module Arg.Add (AddOpts (AddOpts)) where

data AddOpts = AddOpts
  { desc :: String
  }
  deriving (Show)
