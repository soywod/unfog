module Main where

import ArgParser
import qualified Command (handle)
import qualified Procedure (handle)
import qualified Query (handle)

main :: IO ()
main = parseArgs >>= dispatch
  where
    dispatch (QueryArg arg) = Query.handle arg
    dispatch (CommandArg arg) = Command.handle arg
    dispatch (ProcedureArg arg) = Procedure.handle arg
