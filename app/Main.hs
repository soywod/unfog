module Main where

import ArgParser
import qualified Command as Cmd
import qualified Parsec
import qualified Query as Qry
import System.Environment (getArgs)

main :: IO ()
main = dispatch . mapToParsecArg =<< parseArgs
  where
    dispatch args = case Parsec._type args of
      Parsec.Cmd -> Cmd.handle args
      Parsec.Qry -> Qry.handle args
