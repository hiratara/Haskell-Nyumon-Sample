{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main(main) where

f' :: Maybe Int
f' = Just 10 >>= \x -> Nothing >>= \_ -> return (x * 2)

f :: Maybe Int
f = do
  x <- Just 10
  Nothing
  return $ x * 2

main :: IO ()
main = do
  print $ f'
  print $ f
