module Main where

import Arg.Parser
import qualified Command
import qualified Query

main :: IO ()
main = parseArgs >>= dispatch

dispatch :: Arg -> IO ()
dispatch (List opts) = putStrLn $ show opts
dispatch (Info opts) = putStrLn $ show opts
dispatch (Wtime opts) = putStrLn $ show opts
dispatch (Status opts) = putStrLn $ show opts
dispatch Upgrade = putStrLn "upgrade"
dispatch Version = putStrLn "version"
dispatch (Add opts) = putStrLn $ show opts
dispatch (Edit opts) = putStrLn $ show opts
dispatch (Start opts) = putStrLn $ show opts
dispatch (Stop opts) = putStrLn $ show opts
dispatch (Do opts) = putStrLn $ show opts
dispatch (Undo opts) = putStrLn $ show opts
