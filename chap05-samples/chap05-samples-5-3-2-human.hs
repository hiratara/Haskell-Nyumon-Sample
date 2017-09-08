{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main(main) where

data Gender = Man | Woman deriving Show
data Human = Human
  { name :: String
  , age :: Int
  , gender :: Gender
  } deriving Show

main :: IO ()
main = do
  print $ Human "Taro" 10 Man -- 通常の使い方
  print $ Human <$> Just "Taro" <*> pure 10 <*> pure Man
  -- Nothingを適用しようとすると、結果がNothingになる
  print $ Human <$> Just "Taro" <*> Nothing <*> pure Man
