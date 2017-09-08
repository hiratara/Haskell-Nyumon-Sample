module Main(main) where

import Data.Char

main :: IO () -- Control-Cなどで中断
main = do
  xs <- getContents
  putStr $ map toUpper xs
