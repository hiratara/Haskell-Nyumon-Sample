module Main(main) where

points :: [(Integer, Integer)]
points = do
  x <- [1..3]
  y <- [1..3]
  return (x, y)

main :: IO ()
main = print points

