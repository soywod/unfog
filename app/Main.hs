module Main where

import Unfog.ArgParser
import qualified Unfog.Command as Command
import qualified Unfog.Procedure as Procedure
import qualified Unfog.Query as Query

main :: IO ()
main = parseArgs >>= dispatch
  where
    dispatch (QueryArg arg) = Query.handle arg
    dispatch (CommandArg arg) = Command.handle arg
    dispatch (ProcedureArg arg) = Procedure.handle arg
