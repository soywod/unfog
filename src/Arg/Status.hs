module Arg.Status (StatusOpts (StatusOpts)) where

data StatusOpts = StatusOpts
  { moreOpt :: Bool,
    jsonOpt :: Bool
  }
  deriving (Show)
