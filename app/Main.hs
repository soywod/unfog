module Main where

import           System.Environment             ( getArgs )
import           Dispatch                       ( dispatch )

main :: IO ()
main = getArgs >>= dispatch
