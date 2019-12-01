module Main where

import           Data.Time.Clock.POSIX          ( getCurrentTime )
import           Data.List                      ( (\\) )
import           System.Environment             ( getArgs )

import           Store
import           State                          ( applyEvents )
import qualified Command                       as Cmd
import qualified Query                         as Qry
import           Response
import           Parsec                         ( ArgTree(..)
                                                , parseArgs
                                                )


main :: IO ()
main = getArgs >>= dispatch . parseArgs . unwords
-- main = do
--   now  <- getCurrentTime
--   args <- parseArgs . unwords <$> getArgs
--   evts <- readEvents
--   let state = applyEvents evts
--   let cmd   = _cmd args
--   let evts  = execute state cmd
--   -- let subscribers = [logger]
--   writeEvents evts
--   -- notify rtype command subscribers

dispatch :: ArgTree -> IO ()
dispatch args = case Parsec._cmd args of
  "create" -> Cmd.handle args
  -- "add"         -> C.handle rtype $ "create" : args
  -- "update"      -> C.handle rtype $ "update" : args
  -- "edit"        -> C.handle rtype $ "update" : args
  -- "replace"     -> C.handle rtype $ "replace" : args
  -- "start"       -> C.handle rtype $ "start" : args
  -- "stop"        -> C.handle rtype $ "stop" : args
  -- "toggle"      -> C.handle rtype $ "toggle" : args
  -- "done"        -> C.handle rtype $ "done" : args
  -- "delete"      -> C.handle rtype $ "delete" : args
  -- "remove"      -> C.handle rtype $ "remove" : args
  -- "context"     -> C.handle rtype $ "context" : args

  -- "list"        -> Q.handle rtype $ "list" : args
  -- "show"        -> Q.handle rtype $ "show" : args
  -- "wtime"       -> Q.handle rtype $ "worktime" : args
  -- "worktime"    -> Q.handle rtype $ "worktime" : args

  -- (command : _) -> printErr rtype $ command ++ ": command not found"
  -- []            -> printErr rtype "command missing"
 -- where
  -- argTree = parseArgs args
  -- options = ["--json"]
  -- rtype   = getResponseType args
