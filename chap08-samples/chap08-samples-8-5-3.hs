module Main where

import Control.Monad (forever)
import Control.Concurrent
-- Control.Exceptionの代わりにControl.Exception.Safeをimportしても実行できる
import Control.Exception (mask, try, throwIO, finally, Exception, SomeException)
import Data.Typeable

data MyException = ThisException | ThatException deriving (Show, Typeable)
instance Exception MyException

main :: IO ()
main = do
  putStrLn "MAIN: Begin"
  mv <- newEmptyMVar
  tid <- forkIO $ do
    putStrLn "Thread1: new"
    throwCatchLoop `finally` (putStrLn "Thread1: finally cleanup" >> putMVar mv ())

  threadDelay 3000000
  putStrLn "MAIN: killThread"
  killThread tid
  takeMVar mv
  putStrLn "MAIN: Done"

throwCatchLoop :: IO ()
throwCatchLoop = mask $ \restore -> forever $ do
  ei <- try $ restore $ do
    threadDelay 500000
    throwIO $ ThisException :: IO ()
    return ()
  case ei :: Either SomeException () of
    Right v -> print v
    Left e -> putStrLn $ "Thread1: Caught exception (" ++ show e ++ ")"
