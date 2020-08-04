module Main where

import Arg.Parser
import qualified Command.Handler as Command
import qualified Query.Handler as Query

main :: IO ()
main = parseArgs >>= dispatch
  where
    dispatch (QueryArg arg) = Query.handle arg
    dispatch (CommandArg arg) = Command.handle arg
