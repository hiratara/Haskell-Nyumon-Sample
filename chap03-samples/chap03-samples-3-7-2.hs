module Main (main) where

newtype NTIndexed a = NewNTIndexed { unNTIndexed :: (Integer, a) }
                      deriving Show

x, y :: NTIndexed String
x = NewNTIndexed (10, "ten")
y = NewNTIndexed (12, "twelve")

main :: IO ()
main = do
  -- xはタプルではないのでエラーとなる
  -- fst x

  let NewNTIndexed x' = x
  print $ fst x'

  -- 引数xがそのまま返される
  print $ NewNTIndexed (unNTIndexed x)
  -- 引数(2, "two")がそのまま返される
  print $ unNTIndexed (NewNTIndexed (2, "two"))

  print $ snd (unNTIndexed y)
