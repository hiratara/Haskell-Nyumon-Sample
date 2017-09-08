module Main(main) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newListArray, readArray, writeArray, getElems)

doubleArray :: [Double]
doubleArray = runST $ do
  arr <- newListArray (0, 4) [1..5] :: ST s (STUArray s Int Double)
  x <- readArray arr 2
  writeArray arr 2 (x * 10.0)
  getElems arr

main :: IO ()
main = print doubleArray
