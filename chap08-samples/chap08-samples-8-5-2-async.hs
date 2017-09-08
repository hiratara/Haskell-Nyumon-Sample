module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)

main :: IO ()
main = do
  putStrLn "Using async"
  _ <- async $ do
    _ <- async $ forever $ do
      putStrLn "Thread2: Can you hear me?"
      threadDelay 500000 -- 0.5sec
    threadDelay 1000000 -- 1sec
    putStrLn "Thread1: end"

  threadDelay 3000000 -- 3sec
