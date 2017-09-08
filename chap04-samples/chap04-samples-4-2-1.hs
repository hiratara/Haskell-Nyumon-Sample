module Main(main) where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print args
