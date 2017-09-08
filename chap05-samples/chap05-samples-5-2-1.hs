module Main(main) where

import Control.Monad

main :: IO ()
main = do
  forM_ [(1 :: Int)..9] $ \x -> do
    forM_ [(1 :: Int)..9] $ \y -> do
      putStr $ show (x * y) ++ "\t"
    putStrLn ""
