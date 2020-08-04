module Main where

import ArgParser
import qualified Command
import qualified Procedure
import qualified Query

main :: IO ()
main = parseArgs >>= dispatch
  where
    dispatch (QueryArg arg) = Query.handle arg
    dispatch (CommandArg arg) = Command.handle arg
    dispatch (ProcedureArg arg) = Procedure.handle arg
