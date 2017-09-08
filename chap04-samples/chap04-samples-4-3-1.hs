module Main(main) where

main :: IO ()
main = do
  x <- getChar
  print x
  x <- getChar
  print x
  x <- getChar
  print x
  xs <- getLine
  putStrLn xs
