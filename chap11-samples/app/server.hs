module Main where

import qualified Data.Text.IO as T
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe
import System.IO (stdout)
import Auction.Types
import Auction.Server
import Supervisor
import Logger

main :: IO ()
main = do
  let port = Port 4000

  auctionState <- newAuctionState

  let logfunc = T.hPutStrLn stdout
  queue <- newTQueueIO -- ロガーで用いるキューの作成
  a1 <- async $ supervisor queue $ logWriter queue logfunc
  a2 <- async $ supervisor queue $ facilitator queue auctionState

  let auctionServer = jsonServer port (auctionService auctionState)
  supervisor queue auctionServer `finally` do
    putStrLn "Shutting down..."
    cancel a2
    cancel a1
