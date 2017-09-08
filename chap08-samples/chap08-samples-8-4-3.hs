{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-omit-yields #-}
module Main where

import System.IO
import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.STM

main = do
  hSetBuffering stdout LineBuffering
  m <- newEmptyMVar
  tvar <- newTVarIO 10 :: IO (TVar Int)

  delay1sec <- registerDelay 1000000

  tid <- forkFinally
    (do
      putStrLn "Child thread..."
      action <- atomically $ do
        modifyTVar' tvar (* 2)
        wait delay1sec
        return $ putStrLn "Multiply Int in TVar by 2"
      action)
    (\case
      Right _ -> putStrLn "Finished the task" >> putMVar m ()
      Left e -> putStrLn ("Killed by Exception: " ++ show e) >> putMVar m ())

  threadDelay 1000
  killThread tid
  readTVarIO tvar >>= \n -> putStrLn $ "TVar: " ++ show n
  takeMVar m

wait :: TVar Bool -> STM ()
wait delay = do
  ok <- readTVar delay
  when (not ok) retry
