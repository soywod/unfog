module Main where

import Unfog.Arg.Parser as Arg (parse)
import Unfog.Arg.Types (Arg (CommandArg, ProcedureArg, QueryArg))
import Unfog.Command as Command (handle)
import Unfog.Procedure as Procedure (handle)
import Unfog.Query as Query (handle)

main :: IO ()
main = dispatch =<< Arg.parse
  where
    dispatch (CommandArg arg) = Command.handle arg
    dispatch (ProcedureArg arg) = Procedure.handle arg
    dispatch (QueryArg arg) = Query.handle arg
