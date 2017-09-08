{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables, GADTs #-}
module Auction.Server (jsonServer, Port(..), auctionService, facilitator) where

import Prelude hiding (id)
import Control.Monad (void)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.), (.~), (&))
import qualified Data.Text as T
import Data.Aeson
import Data.Time.Clock
import qualified Data.HashMap.Strict as M
import Web.Scotty as Scotty (scotty, post, body, json, status)
import Network.HTTP.Types.Status (badRequest400)
import Auction.Types


newtype Port = Port Int deriving Show

jsonServer :: (FromJSON request, ToJSON response) => Port -> (request -> IO response) -> IO ()
jsonServer (Port port) requestHandler = scotty port $ do
    Scotty.post "/api" $ do
        reqBody <- body
        case decode reqBody of
            Just request -> do
                response <- liftIO $ requestHandler $ request
                Scotty.json response
            Nothing -> status badRequest400

auctionService :: AuctionState -> AuctionServerRequest -> IO AuctionServerResponse
auctionService auctionState (AuctionServerRequest request) =
    requestHandler
        `catch` \(e :: AuctionException) -> return (AuctionErr e)
        `catch` \(_ :: SomeException) -> return (AuctionErr UnknownError)
  where
    requestHandler = case checkInput request of
        Err msg -> return $ AuctionErr (BadData msg)
        Ok -> case request of
            ViewAuctionItemReq -> do
                result <- evalAuction auctionState ViewAuctionItem
                return $ AuctionOk (ViewAuctionItemRes result)
            RegisterUserReq newUser -> do
                result <- evalAuction auctionState (RegisterUser newUser)
                return $ AuctionOk (RegisterUserRes result)
            CheckUserReq uid -> do
                result <- evalAuction auctionState (CheckUser uid)
                return $ AuctionOk (CheckUserRes result)
            RegisterItemReq uid item -> do
                result <- evalAuction auctionState (RegisterItem uid item)
                return $ AuctionOk (RegisterItemRes result)
            BidReq uid price -> do
                result <- evalAuction auctionState (Bid uid price)
                return $ AuctionOk (BidRes result)
            SellToAuctionReq uid item term price -> do
                result <- evalAuction auctionState (SellToAuction uid item term price)
                return $ AuctionOk (SellToAuctionRes result)

getUser :: AuctionState -> UserId -> STM (Maybe (TVar User))
getUser state uid = do
    userMap <- readTVar $ registeredUsers state
    return $! M.lookup uid userMap

getLoginUser :: AuctionState -> UserId -> IO (TVar User)
getLoginUser state uid = do
    atomically (getUser state uid) >>= \case
        Just tUser -> return tUser
        Nothing -> throwIO UserNotFound

updateUser :: TVar User -> (User -> User) -> STM ()
updateUser tUser change = do
    oldUser <- readTVar tUser
    let changedUser = change oldUser
    if changedUser ^. id == oldUser ^. id
        then writeTVar tUser changedUser
        else throwIO InvalidUserUpdate

registerUser :: AuctionState -> User -> IO ()
registerUser state user = atomically $ do
    let users = registeredUsers state
    tUser <- newTVar user
    modifyTVar' users (\userMap -> M.insert (user ^. id) tUser userMap)

checkInput :: AuctionRequest -> CheckResult
checkInput ViewAuctionItemReq = Ok
checkInput (RegisterUserReq user) = if length (user ^. name) > 0 then Ok else Err "User name is required"
checkInput (CheckUserReq _uid) = Ok
checkInput (RegisterItemReq _uid item) = if length (item ^. name) > 0 && length (item ^. description) > 0
    then Ok
    else Err "Item name and description should not be empty"
checkInput (BidReq _uid price) = if price > 0
    then Ok
    else Err "Bidding price must be positive"
checkInput (SellToAuctionReq _uid _item term price) =
    if startTime term < endTime term
        then if price >= 0
            then Ok
            else Err "First price must be non-negative"
        else Err "Invalid term"

--------------------------------------------------------------------------------
evalAuction :: AuctionState -> Auction a -> IO a
evalAuction state ViewAuctionItem = do
    mtAuctionItem <- atomically $ tryReadTMVar (currentAuctionItem state)
    case mtAuctionItem of
        Just tAuctionItem -> do
            auctionItem <- toAuctionItem tAuctionItem
            return $ Just auctionItem
        Nothing -> return Nothing
evalAuction state (RegisterUser newUser) = do
    user <- buildUser newUser
    registerUser state user
    return $ user ^. id
evalAuction state (CheckUser uid) = getLoginUser state uid >>= readTVarIO
evalAuction state (RegisterItem uid ni) = do
    tUser <- getLoginUser state uid
    item <- buildItem ni
    atomically $ addItemToUser tUser item
evalAuction state (Bid uid bidPrice) = do
  tUser <- getLoginUser state uid
  currentTime <- getCurrentTime
  atomically $ do
    user <- readTVar tUser
    -- 金額が基準に達しているか確認。
    if user ^. money < bidPrice
      then throwIO NoEnoughMoney
      else do
        mAuctionItem <- tryTakeTMVar $ currentAuctionItem state
        case mAuctionItem of
          Nothing -> throwIO NoAuctionItem
          Just auctionItem ->
            -- オークションにビッド可能か確認。
            if checkInTerm currentTime (auctionItem ^. auctionTerm)
              -- ビッド額と現在価格の比較。
              then if bidPrice <= (auctionItem ^. currentPrice)
                then throwIO LowPrice
                else do
                  let auctionItem' = auctionItem
                        & currentPrice .~ bidPrice
                        & currentUser .~ Just tUser
                  putTMVar (currentAuctionItem state) auctionItem'
              else throwIO OutOfTerm
evalAuction state (SellToAuction uid item term price) = do
    tUser <- getLoginUser state uid
    currentTime <- getCurrentTime
    if startTime term < endTime term && currentTime < endTime term
        then if price < 0
            then throwIO InvalidFirstPrice
            else do
                _ <- putItemToAuction state tUser item term price
                return ()
        else throwIO InvalidTerm

-- ユーザにアイテムを渡す。
addItemToUser :: TVar User -> Item -> STM ItemId
addItemToUser tUser item = do
  updateUser tUser
      (\user -> user & inventory .~ addItem (user ^. inventory) item)
  return $ item ^. id

-- ユーザのもつアイテムを減らしてオークション商品として登録する。
putItemToAuction :: AuctionState -> TVar User -> Item -> Term -> Price -> IO AuctionItem'
putItemToAuction state tUser item term price = do
  aiid <- newAuctionItemId
  atomically $ do
    user <- readTVar tUser
    let Inventory inventoryItems = user ^. inventory
    if item ^. id `elem` fmap (^. id) inventoryItems
        then do
          -- ユーザのインベントリからアイテムを削除する。
          let inventoryItems' =
                filter (\item' -> item' ^. id /= item ^. id)
                  inventoryItems
          let user' = user & inventory .~ Inventory inventoryItems'
          writeTVar tUser user'
          mAuctionItem <- tryTakeTMVar $ currentAuctionItem state
          case mAuctionItem of
            Just _ -> throwIO AuctionItemAlreadyExist
            Nothing -> do
              -- オークション商品に登録。
              let tAuctionItem =
                    AuctionItem' aiid tUser price Nothing term item
              putTMVar (currentAuctionItem state) tAuctionItem
              return tAuctionItem
        else throwIO ItemNotFound

-- TQueueはSTMによるキュー。ロギングのために用いる。
facilitator :: TQueue T.Text -> AuctionState -> IO ()
facilitator queue state = loop
  where
    loop = do
      threadDelay $ 1000 * 1000 -- 1sec
      handleFinishedAuctionItem queue state
      loop

handleFinishedAuctionItem :: TQueue T.Text -> AuctionState -> IO ()
handleFinishedAuctionItem queue state = do
  currentTime <- getCurrentTime
  atomically $ do
    -- オークションが開催されているかどうか確認する。
    mAuctionItem <- tryReadTMVar (currentAuctionItem state)
    case mAuctionItem of
      Nothing -> writeTQueue queue "FACILITATOR: Auction doesn't hold"
      Just auctionItem -> if currentTime < endTime (auctionItem ^. auctionTerm)
        then writeTQueue queue "FACILITATOR: Auction holds"
        else do
          -- オークションを終わらせる。
          void $ takeTMVar (currentAuctionItem state)
          case auctionItem ^. currentUser of
            Just tWinner -> do
              -- winnerの所持金を減らし、アイテムを渡す。
              updateUser tWinner
                (\winner ->
                   winner & inventory .~ addItem
                              (winner ^. inventory)
                              (auctionItem ^. auctionTargetItem)
                          & money .~
                              (winner ^. money) - (auctionItem ^. currentPrice))

              let tSeller = auctionItem ^. seller
              -- sellerの所持金を増やす。
              updateUser tSeller
                (\sellUser ->
                    sellUser & money .~
                        (sellUser ^. money) + (auctionItem ^. currentPrice))
              writeTQueue queue "FACILITATOR: Auction finished successfully"
            Nothing -> do -- bidderがいない場合、sellerにアイテムを返す。
              let tSeller = auctionItem ^. seller
              void $ addItemToUser tSeller (auctionItem ^. auctionTargetItem)
              writeTQueue queue "FACILITATOR: Auction finished with no bidder"
