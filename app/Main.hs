module Main where

import Arg.Parser
import qualified Command
import qualified Query

main :: IO ()
main = dispatch =<< parseArgs

dispatch :: Arg -> IO ()
dispatch (List opts) = putStrLn $ "list " ++ show opts
dispatch (Info opts) = putStrLn $ "info " ++ show opts
dispatch (Wtime opts) = putStrLn $ "wtime " ++ show opts
