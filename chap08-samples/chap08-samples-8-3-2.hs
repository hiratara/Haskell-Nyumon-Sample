module Main where

import Control.Applicative
import Control.Concurrent (forkIO, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.STM -- stmパッケージで定義

main = do
  putStrLn "Begin"
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar

  account1 <- newTVarIO (10000 :: Int)
  account2 <- newTVarIO (10000 :: Int)

  let wait b = case b of
        0 -> return 1
        1 -> return 1
        n -> (+) <$> wait (n - 1) <*> wait (n - 2)

  -- Thread 1
  forkIO $ do
    atomically $ do
      balance1 <- readTVar account1
      balance2 <- readTVar account2
      wait 35
      writeTVar account1 (balance1 + 1000)
      writeTVar account2 (balance2 - 1000)
    putMVar mv1 () -- 処理が終了したことをメインスレッドへ通知

  -- Thread 2
  forkIO $ do
    atomically $ do
      wait 32
      balance1 <- readTVar account1
      balance2 <- readTVar account2
      writeTVar account1 (balance1 - 2000)
      writeTVar account2 (balance2 + 2000)
    putMVar mv2 () -- 処理が終了したことをメインスレッドへ通知

  -- タスクが終わるまで待つ
  takeMVar mv1
  takeMVar mv2

  balances <- atomically $ (,) <$> readTVar account1 <*> readTVar account2
  print balances
  putStrLn "Done"
