module Main where

import           Data.List                      ( (\\) )
import           System.Environment             ( getArgs )

import qualified Command                       as C
import qualified Query                         as Q
import           Response

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch args = case args \\ options of
  ("add"      : args) -> C.handle rtype $ "add" : args
  ("edit"     : args) -> C.handle rtype $ "edit" : args
  ("start"    : args) -> C.handle rtype $ "start" : args
  ("stop"     : args) -> C.handle rtype $ "stop" : args
  ("done"     : args) -> C.handle rtype $ "done" : args
  ("delete"   : args) -> C.handle rtype $ "delete" : args
  ("context"  : args) -> C.handle rtype $ "context" : args
  ("list"     : args) -> Q.handle rtype $ "list" : args
  ("show"     : args) -> Q.handle rtype $ "show" : args
  ("worktime" : args) -> Q.handle rtype $ "worktime" : args
  (command    : _   ) -> printErr rtype $ command ++ ": command not found"
  []                  -> printErr rtype "command missing"
 where
  options = ["--json"]
  rtype   = getResponseType args
