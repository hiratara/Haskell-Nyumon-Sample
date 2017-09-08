module Main(main) where

import Control.Monad (guard)

orderedPoints :: [(Integer, Integer)]
orderedPoints = do
  x <- [1..3]
  y <- [1..3]
  guard (x < y) -- ここで絞り込む。
  return (x, y)

main :: IO ()
main = print orderedPoints

