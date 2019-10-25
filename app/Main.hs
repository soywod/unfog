module Main where

import           System.Environment
import           Dispatch                       ( dispatch )

main :: IO ()
main = getArgs >>= dispatch
