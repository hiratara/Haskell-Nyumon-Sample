module Main(main) where

import Data.Time (getZonedTime)

main :: IO ()
main = do
  zonedTime <- getZonedTime
  print zonedTime
