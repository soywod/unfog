module Main where

import           System.Environment             ( getArgs )

import qualified Command                        ( handle )
import qualified Query                          ( handle )

main :: IO ()
main = getArgs >>= dispatch
 where
  dispatch ("add"  : args) = Command.handle $ "add" : args
  dispatch ("list" : args) = Query.handle $ "list" : args
