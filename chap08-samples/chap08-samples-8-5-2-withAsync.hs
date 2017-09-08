module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, waitEither_)

main :: IO ()
main = do
  putStrLn "Using withAsync"
  withAsync thread1 $ \a1 ->
    withAsync thread2 $ \a2 ->
      waitEither_ a1 a2
  where
  thread1 = do
    threadDelay 2000000 -- 2sec
    putStrLn "Thread1: end"
  thread2 = forever $ do
    putStrLn "Thread2: Can you hear me?"
    threadDelay 500000 -- 0.5sec
