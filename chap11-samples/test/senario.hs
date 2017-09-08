{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where

import Prelude hiding (id)
import Control.Monad (forever)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Lens ((^.), (.~), (&))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import System.IO
import System.Random

import Auction
import Supervisor
import Logger

simpleSinario = do
    authState <- newAuthState
    auctionState <- newAuctionState

    let registerUser name = evalAuth authState auctionState (RegisterUser name)
    let registerItem authKey item = evalAuth authState auctionState (Action authKey (RegisterItem item))
    let checkUser authKey = evalAuth authState auctionState (Action authKey CheckUser)
    let sellToAuction authKey item term firstPrice = evalAuth authState auctionState (Action authKey (SellToAuction item term firstPrice))
    let viewAuctionItem authKey = evalAuth authState auctionState (Action authKey ViewAuctionItem)
    let bid authKey price = evalAuth authState auctionState (Action authKey (Bid price))

    let logfunc = T.hPutStrLn stdout
    queue <- newTQueueIO
    a1 <- async $ logWriter queue logfunc
    a2 <- async $ supervisor queue $ facilitator queue auctionState

    putStrLn "-- RegisterUser"
    aliceKey <- registerUser "Alice"
    print aliceKey
    bobKey <- registerUser "Bob"
    print bobKey

    putStrLn "-- RegisterItem"
    registerItem aliceKey (UnregisteredItem "Alice's Violin" "Good Violine")

    putStrLn "-- CheckUser"
    eUser <- try @_ @AuctionException $ checkUser aliceKey
    case eUser of
        Left e -> print e
        Right user -> do
            print user
            let Inventory items = user ^. inventory
            let item = head items
            currentTime <- getCurrentTime
            let startTime = currentTime
            let endTime = addUTCTime 1 currentTime
            let term = Term startTime endTime
            let firstPrice = 200
            putStrLn "-- SellToAction"
            sellToAuction aliceKey item term firstPrice

            putStrLn "-- ViewAuctionItem"
            mAuctionItem <- viewAuctionItem aliceKey
            print mAuctionItem
            putStrLn "-- Bid"
            eResult <- try @_ @AuctionException $ bid bobKey 400
            case eResult of
                Left e -> print e
                Right _ -> do
                    putStrLn "-- ViewAuctionItem"
                    mAuctionItem <- viewAuctionItem aliceKey
                    print mAuctionItem

                    putStrLn "-- CheckUser 1 (Alice)"
                    print =<< (try @_ @AuctionException $ checkUser aliceKey)
                    putStrLn "-- CheckUser 1 (Bob)"
                    print =<< (try @_ @AuctionException $ checkUser bobKey)

                    threadDelay $ 2 * 1000000

                    putStrLn "-- CheckUser 2 (Alice)"
                    print =<< (try @_ @AuctionException $ checkUser aliceKey)
                    putStrLn "-- CheckUser 2 (Bob)"
                    print =<< (try @_ @AuctionException $ checkUser bobKey)
    cancel a2
    cancel a1

supervisorSinario = do
    let log = T.hPutStrLn stdout
    queue <- newTQueueIO
    loggerAsync <- async $ logWriter queue log
    supervisorAsync <- async $ supervisor queue $ forever $ do
        threadDelay $ 5000
        rand :: Int <- randomRIO (1, 10)
        if rand <= 3 then error "SuddonDeath!!" else putStrLn "WORKER: heyhey!"

    rand :: Int <- randomRIO (100000, 130000)
    threadDelay $ rand
    cancel supervisorAsync
    threadDelay 100000
    cancel loggerAsync
    putStrLn "END"

main = do
    simpleSinario
    supervisorSinario
