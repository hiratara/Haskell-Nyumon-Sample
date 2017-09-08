module Main(main) where

import System.Directory

main :: IO ()
main = do
  current <- getCurrentDirectory
  findFiles [current ++ "/.." , current] "target.txt" >>= print
  findFile [current ++ "/.." , current] "target.txt" >>= print
