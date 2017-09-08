{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

data LazyAndStrict = LazyAndStrict
                   { lsLazy   :: Int
                   , lsStrict :: !Int
                   }

main :: IO ()
main = do
  print $ lsStrict $ LazyAndStrict undefined 2 -- 第1引数は非正格
  -- 正格評価のためエラーとなる
  -- print $ lsLazy $ LazyAndStrict 1 undefined   -- 第2引数は正格
