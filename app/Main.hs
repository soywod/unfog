module Main where

import           System.Environment             ( getArgs )

import qualified Command                        ( handle )
import qualified Query                          ( handle )

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch args = case args of
  ("add"    : args) -> Command.handle $ "add" : args
  ("edit"   : args) -> Command.handle $ "edit" : args
  ("start"  : args) -> Command.handle $ "start" : args
  ("stop"   : args) -> Command.handle $ "stop" : args
  ("done"   : args) -> Command.handle $ "done" : args
  ("delete" : args) -> Command.handle $ "delete" : args
  ("list"   : args) -> Query.handle $ "list" : args
  ("show"   : args) -> Query.handle $ "show" : args
  (command  : _   ) -> putStrLn $ "unfog: " ++ command ++ ": command not found"
  []                -> putStrLn "unfog: command missing"
