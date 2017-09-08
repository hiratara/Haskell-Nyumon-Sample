module Main where

import Control.Monad.Par

main = do
  print $ parquicksort 4 [14, 43, 43, 4, 5, 654, 3, 4]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort greater
  where
    smaller = filter (< x) xs
    greater = filter (>= x) xs

parquicksort :: Int -> [Int] -> [Int] -- 第一引数で深さ閾値を渡す
parquicksort maxdepth list = runPar $ generate 0 list
  where
    generate :: Int -> [Int] -> Par [Int]
    generate _ [] = return []
    generate d l@(x:xs)
      | d >= maxdepth = return $ quicksort l
      | otherwise = do
        iv1 <- spawn $ generate (d + 1) (filter (< x) xs) -- 並列に式の評価を開始
        iv2 <- spawn $ generate (d + 1) (filter (>= x) xs) -- 並列に式の評価を開始
        lt <- get iv1 -- iv1の実行を待って値を取り出す
        gte <- get iv2 -- iv2の実行を待って値を取り出す
        return $ lt ++ [x] ++ gte
