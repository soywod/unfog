module Main where

import qualified Unfog.Arg.Parser as Arg (parse)
import Unfog.Arg.Types (Arg (CommandArg, ProcedureArg, QueryArg))
import qualified Unfog.Command as Command (handle)
import qualified Unfog.Procedure as Procedure (handle)
import qualified Unfog.Query.Handler as Query (handle)

main :: IO ()
main = dispatch =<< Arg.parse
  where
    dispatch (CommandArg arg) = Command.handle arg
    dispatch (ProcedureArg arg) = Procedure.handle arg
    dispatch (QueryArg arg) = Query.handle arg
