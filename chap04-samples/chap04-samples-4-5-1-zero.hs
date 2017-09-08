module Main(main) where

import Control.Exception

catchZeroDiv :: ArithException -> IO Int
catchZeroDiv DivideByZero = return 0
catchZeroDiv e = throwIO e

main :: IO ()
main = do
  n1 <- (return $ 100 `div` 0) `catch` catchZeroDiv
  print n1
  n2 <- (return $! 100 `div` 0) `catch` catchZeroDiv
  print n2
