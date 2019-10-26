module Main where

import           System.Environment             ( getArgs )
import           Dispatch                       ( handleArgs )

main :: IO ()
main = getArgs >>= handleArgs
