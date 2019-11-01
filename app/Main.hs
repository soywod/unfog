module Main where

import           System.Environment             ( getArgs )

import qualified Command                        ( handle )
import qualified Query                          ( handle )
import           Utils                          ( elog )

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch args = case args of
  ("add"      : args) -> Command.handle $ "add" : args
  ("edit"     : args) -> Command.handle $ "edit" : args
  ("start"    : args) -> Command.handle $ "start" : args
  ("stop"     : args) -> Command.handle $ "stop" : args
  ("done"     : args) -> Command.handle $ "done" : args
  ("delete"   : args) -> Command.handle $ "delete" : args
  ("context"  : args) -> Command.handle $ "context" : args
  ("list"     : args) -> Query.handle $ "list" : args
  ("show"     : args) -> Query.handle $ "show" : args
  ("worktime" : args) -> Query.handle $ "worktime" : args
  (command    : _   ) -> elog command "command not found"
  []                  -> elog "" "command missing"
