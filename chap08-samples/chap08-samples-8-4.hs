{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent

main :: IO ()
main = do
  putStrLn "BEGIN"
  mv <- newEmptyMVar

  tid <- forkFinally
    -- スレッドでの処理
    (do
      tid <- myThreadId
      print tid
      threadDelay 2000000 -- 2sec
      putStrLn "Hey!")
    -- スレッドのファイナライザ、処理を場合分け
    (\case
      Right _ -> do
        putStrLn "Finished the task"
        putMVar mv ()
      Left e -> do
        putStrLn $ "Killed by Exception: " ++ show e
        putMVar mv ())

  threadDelay 1000000 -- 1sec
  killThread tid -- 非同期例外を投げる
  takeMVar mv
  putStrLn "END"
