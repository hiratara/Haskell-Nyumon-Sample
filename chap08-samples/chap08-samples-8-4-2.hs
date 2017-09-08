{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-omit-yields #-}
module Main where

import Control.Concurrent
import Control.Exception

main :: IO ()
main = do
  m <- newEmptyMVar

  tid <- forkFinally
    (mask $ \unmask -> do
      let showResult v = putStrLn $ "Result: " ++ show v
      putStrLn "Child thread..."
      evaluate (fib 40) >>= showResult -- マスクされた時間のかかる処理1
      evaluate (fib 39) >>= showResult -- マスクされた時間のかかる処理2
      unmask (evaluate $ fib 38) >>= showResult -- マスクされていない処理
      putStrLn "Hey!")
    (\case
      Right _ -> putStrLn "Finished the task" >> putMVar m ()
      Left e -> putStrLn ("Killed by Exception: " ++ show e) >> putMVar m ())

  threadDelay 1000
  killThread tid -- tidに非同期例外を投げる。スレッドキル
  takeMVar m

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
