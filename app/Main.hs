module Main where

import Arg.Parser
import Command.Handler
import Query.Handler

main :: IO ()
main = parseArgs >>= dispatch
  where
    dispatch (QueryArg query) = handleQuery query
    dispatch (CommandArg command) = handleCommand command
