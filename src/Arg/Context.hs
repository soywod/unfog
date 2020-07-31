module Arg.Context (CtxOpts (CtxOpts)) where

import Task (Tag)

data CtxOpts = CtxOpts
  { tags :: [Tag]
  }
  deriving (Show)
