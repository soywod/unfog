module Main where

import           System.Environment             ( getArgs )

import qualified Parsec
import qualified Command                       as Cmd
import qualified Query                         as Qry

main :: IO ()
main = getArgs >>= dispatch . Parsec.parseArgs . unwords
 where
  dispatch args = case Parsec._type args of
    Parsec.Cmd -> Cmd.handle args
    Parsec.Qry -> Qry.handle args
