{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Auction.Types where

import Prelude hiding (id)
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.UUID (UUID, toString, fromString)
import Data.UUID.V4 as UUID.V4
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.Time


-- IDや金額、価格
newtype ItemId = ItemId UUID deriving (Show, Eq, Ord)
newtype UserId = UserId UUID deriving (Eq, Ord, Show, Hashable)
newtype Money = Money Int deriving (Show, Read, Eq, Ord, Num)
newtype AuctionItemId = AuctionItemId UUID deriving (Show, Eq, Ord)
data Term = Term { startTime :: UTCTime, endTime :: UTCTime } deriving Show
type Price = Money

-- アイテム
data NewItem = NewItem
    { _newItemName :: String
    , _newItemDescription :: String
    } deriving Show
makeFields ''NewItem

data Item = Item
    { _itemId :: ItemId
    , _itemName :: String
    , _itemDescription :: String
    } deriving Show
makeFields ''Item

-- 在庫
newtype Inventory = Inventory [Item] deriving Show

-- ユーザ
data NewUser = NewUser
    { _newUserName :: String
    , _newUserMoney :: Money
    } deriving Show
makeFields ''NewUser

data User = User
    { _userId :: UserId
    , _userName :: String
    , _userInventory :: Inventory
    , _userMoney :: Money
    }
    deriving Show
makeFields ''User

buildUser :: NewUser -> IO User
buildUser newUser = do
    uuid <- UUID.V4.nextRandom
    let userId = UserId uuid
    let emptyInventory = Inventory []
    let user = User userId (newUser ^. name) emptyInventory (newUser ^. money)
    return user

addItem :: Inventory -> Item -> Inventory
addItem (Inventory items) item = Inventory (item:items)

buildItem :: NewItem -> IO Item
buildItem item = do
    uuid <- UUID.V4.nextRandom
    return $ Item (ItemId uuid) (item ^. name) (item ^. description)

checkInTerm :: UTCTime -> Term -> Bool
checkInTerm time term = startTime term <= time && time < endTime term

newAuctionItemId :: IO AuctionItemId
newAuctionItemId = do
    uuid <- UUID.V4.nextRandom
    return $ AuctionItemId uuid

userIdToString :: UserId -> String
userIdToString (UserId uuid) = toString uuid

stringToUserId :: String -> Maybe UserId
stringToUserId str = UserId <$> fromString str

--------------------------------------------------------------------------------
-- オークション商品（TVarを利用するためにシリアライズ不可能。）
data AuctionItem' = AuctionItem'
    { _auctionItem'AuctionItemId :: AuctionItemId
    , _auctionItem'Seller :: TVar User
    , _auctionItem'CurrentPrice :: Price
    , _auctionItem'CurrentUser :: Maybe (TVar User)
    , _auctionItem'AuctionTerm :: Term
    , _auctionItem'AuctionTargetItem :: Item
    }
makeFields ''AuctionItem'

-- オークション商品（シリアライズ用）
data AuctionItem = AuctionItem
    { _auctionItemAuctionItemId :: AuctionItemId
    , _auctionItemSellerId :: UserId
    , _auctionItemCurrentPrice :: Price
    , _auctionItemCurrentUserId :: Maybe UserId
    , _auctionItemAuctionTerm :: Term
    , _auctionItemAuctionTargetItem :: Item
    }
    deriving Show
makeFields ''AuctionItem

toAuctionItem :: AuctionItem' -> IO AuctionItem
toAuctionItem tAuctionItem = do
    sellUser <- atomically $ readTVar (tAuctionItem ^. seller)
    let mtCurrentUser = (tAuctionItem ^. currentUser)
    mCurrentUserId <- case mtCurrentUser of
        Just tCurrentUser -> do
            user <- atomically $ readTVar tCurrentUser
            return $ Just (user ^. id)
        Nothing -> return Nothing

    return $ AuctionItem
        { _auctionItemAuctionItemId = tAuctionItem ^. auctionItemId
        , _auctionItemSellerId = sellUser ^. id
        , _auctionItemCurrentPrice = tAuctionItem ^. currentPrice
        , _auctionItemCurrentUserId = mCurrentUserId
        , _auctionItemAuctionTerm = tAuctionItem ^. auctionTerm
        , _auctionItemAuctionTargetItem = tAuctionItem ^. auctionTargetItem
        }

--------------------------------------------------------------------------------
data Auction a where
    ViewAuctionItem :: Auction (Maybe AuctionItem) -- 商品の参照
    RegisterUser :: NewUser -> Auction UserId -- ユーザ登録
    CheckUser :: UserId -> Auction User -- ユーザ情報取得
    RegisterItem :: UserId -> NewItem -> Auction ItemId -- アイテムの登録
    Bid :: UserId -> Price -> Auction () -- 入札
    SellToAuction :: UserId -> Item -> Term -> Price -> Auction () -- 出品

data AuctionRequest
    = ViewAuctionItemReq
    | RegisterUserReq NewUser
    | CheckUserReq UserId
    | RegisterItemReq UserId NewItem
    | BidReq UserId Price
    | SellToAuctionReq UserId Item Term Price

data AuctionResponse
    = ViewAuctionItemRes (Maybe AuctionItem)
    | RegisterUserRes UserId
    | CheckUserRes User
    | RegisterItemRes ItemId
    | BidRes ()
    | SellToAuctionRes ()

--------------------------------------------------------------------------------
newtype AuctionServerRequest = AuctionServerRequest AuctionRequest
data AuctionServerResponse = AuctionOk AuctionResponse | AuctionErr AuctionException

data AuctionState = AuctionState
    { registeredUsers :: TVar (M.HashMap UserId (TVar User))
    , currentAuctionItem :: TMVar AuctionItem'
    }

newAuctionState :: IO AuctionState
newAuctionState = do
    emptyUsers <- newTVarIO M.empty
    emptyItems <- newEmptyTMVarIO
    return $ AuctionState emptyUsers emptyItems

data CheckResult = Ok | Err String

--------------------------------------------------------------------------------
data AuctionException
    = NoAuctionItem | LowPrice | OutOfTerm | UserNotFound
    | InvalidTerm | InvalidFirstPrice | AuctionItemAlreadyExist
    | InvalidUserUpdate | ItemNotFound | NoEnoughMoney | BadData String
    | UnknownError | NetworkError | DecodeError
    deriving (Show, Typeable)
instance Exception AuctionException

deriveJSON defaultOptions ''Money
deriveJSON defaultOptions ''AuctionItemId
deriveJSON defaultOptions ''AuctionItem
deriveJSON defaultOptions ''ItemId
deriveJSON defaultOptions ''NewItem
deriveJSON defaultOptions ''Item
deriveJSON defaultOptions ''Term
deriveJSON defaultOptions ''NewUser
deriveJSON defaultOptions ''User
deriveJSON defaultOptions ''Inventory
deriveJSON defaultOptions ''UserId
deriveJSON defaultOptions ''UUID

deriveJSON defaultOptions ''AuctionRequest
deriveJSON defaultOptions ''AuctionResponse
deriveJSON defaultOptions ''AuctionServerRequest
deriveJSON defaultOptions ''AuctionServerResponse
deriveJSON defaultOptions ''AuctionException



