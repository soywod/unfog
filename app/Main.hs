module Main where

import           System.Environment             ( getArgs )

import qualified Command                        ( handle )
import qualified Query                          ( handle )

main :: IO ()
main = getArgs >>= dispatch
 where
  dispatch ("add"   : args) = Command.handle $ "add" : args
  dispatch ("edit"  : args) = Command.handle $ "edit" : args
  dispatch ("start" : args) = Command.handle $ "start" : args
  dispatch ("stop"  : args) = Command.handle $ "stop" : args
  dispatch ("done"  : args) = Command.handle $ "done" : args
  dispatch ("list"  : args) = Query.handle $ "list" : args
