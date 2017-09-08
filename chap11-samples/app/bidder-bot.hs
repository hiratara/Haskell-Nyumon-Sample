module Main where

import Prelude hiding (id)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Lens ((^.))
import Auction.Types
import Auction.Client


main :: IO ()
main = do
    uid <- registerUser "bidderbot" 100000
    runAuctionSession (ClientSession uid) bidderBot

type AuctionBot = AuctionSessionT IO

bidderBot :: AuctionBot ()
bidderBot = loop
  where
    loop = do
        liftIO $ threadDelay $ 2 * 1000000
        user <- checkUser
        mAuctionItem <- viewAuctionItem
        case mAuctionItem of
            Nothing -> loop
            Just auctionItem -> if auctionItem ^. currentUserId == Just (user ^. id)
                then loop
                else do
                    bid (auctionItem ^. currentPrice + 100)
                    loop

