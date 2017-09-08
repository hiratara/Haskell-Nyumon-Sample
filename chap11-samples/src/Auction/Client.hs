{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, LambdaCase #-}
module Auction.Client
( evalAuctionOnClient
, AuctionSessionT, ClientSession(..), runAuctionSession
, getCurrentUserId
, registerUser, checkUser, viewAuctionItem, registerItem, sellToAuction, bid
) where

import Data.Aeson
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..), asks)
import Control.Lens
import Control.Exception.Safe
import qualified Network.Wreq as Wreq
import Auction.Types

evalAuctionOnClient :: Auction a -> IO a
evalAuctionOnClient request = evalAuction serverUrl request

evalAuction :: String -> Auction a -> IO a
evalAuction url request = case request of
    ViewAuctionItem -> do
        sendAuctionServerRequest url (AuctionServerRequest ViewAuctionItemReq) >>= \case
            AuctionOk (ViewAuctionItemRes mAuctionItem) -> return mAuctionItem
            AuctionErr e -> throwIO e
            _ -> throwIO UnknownError
    RegisterUser newUser -> do
        sendAuctionServerRequest url (AuctionServerRequest (RegisterUserReq newUser)) >>= \case
            AuctionOk (RegisterUserRes uid) -> return uid
            AuctionErr e -> throwIO e
            _ -> throwIO UnknownError
    CheckUser uid -> do
        sendAuctionServerRequest url (AuctionServerRequest (CheckUserReq uid)) >>= \case
            AuctionOk (CheckUserRes user) -> return user
            AuctionErr e -> throwIO e
            _ -> throwIO UnknownError
    RegisterItem uid item -> do
        sendAuctionServerRequest url (AuctionServerRequest ((RegisterItemReq uid item))) >>= \case
            AuctionOk (RegisterItemRes itemId) -> return itemId
            AuctionErr e -> throwIO e
            _ -> throwIO UnknownError
    Bid uid price -> do
        sendAuctionServerRequest url (AuctionServerRequest (BidReq uid price)) >>= \case
            AuctionOk (BidRes unit) -> return unit
            AuctionErr e -> throwIO e
            _ -> throwIO UnknownError
    SellToAuction uid item term price -> do
        sendAuctionServerRequest url (AuctionServerRequest (SellToAuctionReq uid item term price)) >>= \case
            AuctionOk (SellToAuctionRes unit) -> return unit
            AuctionErr e -> throwIO e
            _ -> throwIO UnknownError

sendAuctionServerRequest :: String -> AuctionServerRequest -> IO AuctionServerResponse
sendAuctionServerRequest url request = do -- URLにJSONエンコードしたリクエストをPOST
    res <- Wreq.post url (encode request)
    case res ^? Wreq.responseBody of -- レスポンスでパターンマッチ
        Just body -> case decode body of
            Just response -> return response
            Nothing -> throwIO DecodeError
        Nothing -> throwIO NetworkError

serverUrl :: String
serverUrl = "http://localhost:4000/api"

--------------------------------------------------------------------------------

data ClientSession = ClientSession
    { csUserId :: UserId
    }

-- モナド変換子 AuctionSessionT
newtype AuctionSessionT m a = AuctionSessionT { unAuctionSessionT :: ReaderT ClientSession m a }
    deriving -- 必要なインスタンスを導出しておく
        ( Functor, Applicative, Monad
        , MonadIO, MonadReader ClientSession, MonadTrans
        , MonadThrow, MonadCatch
        )

runAuctionSession :: Monad m => ClientSession -> AuctionSessionT m a -> m a
runAuctionSession cs m = runReaderT (unAuctionSessionT m) cs

getCurrentUserId :: Monad m => AuctionSessionT m UserId
getCurrentUserId = asks csUserId

--------------------------------------------------------------------------------

registerUser :: MonadIO m => String -> Money -> m UserId
registerUser n m = liftIO $ evalAuctionOnClient $ RegisterUser (NewUser n m)

checkUser :: MonadIO m => AuctionSessionT m User
checkUser = do
    uid <- getCurrentUserId
    liftIO $ evalAuctionOnClient (CheckUser uid)

viewAuctionItem :: MonadIO m => AuctionSessionT m (Maybe AuctionItem)
viewAuctionItem = liftIO $ evalAuctionOnClient ViewAuctionItem

registerItem :: MonadIO m => NewItem -> AuctionSessionT m ItemId
registerItem ni = do
    uid <- getCurrentUserId
    liftIO $ evalAuctionOnClient (RegisterItem uid ni)

sellToAuction :: MonadIO m => Item -> Term -> Price -> AuctionSessionT m ()
sellToAuction i t p = do
    uid <- getCurrentUserId
    liftIO $ evalAuctionOnClient (SellToAuction uid i t p)

bid :: MonadIO m => Price -> AuctionSessionT m ()
bid p = do
    uid <- getCurrentUserId
    liftIO $ evalAuctionOnClient (Bid uid p)
