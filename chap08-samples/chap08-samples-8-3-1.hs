module Main where

import Control.Concurrent

main = do
  putStrLn "Begin"
  account1 <- newMVar 10000
  account2 <- newMVar 10000

  -- Thread 1
  forkIO $ do
    balance1 <- takeMVar account1 -- Success
    threadDelay 1000000
    balance2 <- takeMVar account2 -- Deadlock!!!
    putMVar account1 (balance1 + 1000)
    putMVar account2 (balance2 - 1000)

  -- Thread 2
  forkIO $ do
    balance2 <- takeMVar account2 -- Success
    threadDelay 1000000
    balance1 <- takeMVar account1 -- Deadlock!!!
    putMVar account2 (balance2 + 2000)
    putMVar account1 (balance1 - 2000)

  threadDelay 2000000
  balance1 <- takeMVar account1
  balance2 <- takeMVar account2
  print (balance1, balance2)
  putStrLn "Done"
