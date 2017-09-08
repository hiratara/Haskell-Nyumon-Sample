module Main(main) where

import Control.Exception
import System.IO

main :: IO ()
main =
  bracket (openFile "dummyFileName" ReadMode) hClose $ \h -> do
    s <- hGetContents h
    putStrLn s
