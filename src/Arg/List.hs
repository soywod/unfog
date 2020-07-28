module Arg.List (ListOpts (ListOpts)) where

data ListOpts = ListOpts
  { onlyIdsOpt :: Bool,
    onlyTagsOpt :: Bool,
    moreOpt :: Bool,
    jsonOpt :: Bool
  }
  deriving (Show)
