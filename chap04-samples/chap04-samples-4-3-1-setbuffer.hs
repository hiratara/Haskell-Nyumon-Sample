module Main(main) where

import System.IO (hSetBuffering, stdin, BufferMode(LineBuffering))

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering -- 4.3.4参照
  x <- getChar
  print x
  x <- getChar
  print x
  x <- getChar
  print x
  xs <- getLine
  putStrLn xs
