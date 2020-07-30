module Main where

import Arg.Parser
import qualified Command
import qualified Query

main :: IO ()
main = parseArgs >>= dispatch

dispatch :: Arg -> IO ()
dispatch (List opts) = putStrLn $ "list " ++ show opts
dispatch (Info opts) = putStrLn $ "info " ++ show opts
dispatch (Wtime opts) = putStrLn $ "wtime " ++ show opts
dispatch (Status opts) = putStrLn $ "status " ++ show opts
dispatch Upgrade = putStrLn "upgrade"
dispatch Version = putStrLn "version"
dispatch (Add opts) = putStrLn $ "add" ++ show opts
