{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Exception.Safe
import Control.Concurrent
import Control.Concurrent.STM

import Data.String (IsString)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, encode, decode)
import Data.Aeson.TH (deriveJSON)
import Control.Lens ((^?))
import qualified Network.Wreq as Wreq (post, responseBody)
import qualified Web.Scotty as Scotty (scotty, post, body, json, status)
import Network.HTTP.Types.Status (badRequest400)

--------------------------------------------------------------------------------
data CounterState = CounterState { counter :: TVar Int }

newCounterState :: Int -> IO CounterState
newCounterState n = do
    tv <- newTVarIO n
    return $ CounterState tv

--------------------------------------------------------------------------------
data Counter a where
    Add :: Int -> Counter Int
    Reset :: Counter ()
data CounterRequest = AddRequest Int | ResetRequest
data CounterResponse = AddResponse Int | ResetResponse ()

deriveJSON defaultOptions ''CounterRequest
deriveJSON defaultOptions ''CounterResponse

data CounterError = CounterError String deriving (Show, Typeable)
instance Exception CounterError

newtype Host = Host String deriving (IsString, Show)
newtype Port = Port Int deriving (Num, Show)

--------------------------------------------------------------------------------
-- client
callCounterApi :: Host -> Port -> CounterRequest -> IO CounterResponse
callCounterApi (Host host) (Port p) request = do
    response <- Wreq.post ("http://" <> host <> ":" <> show p <> "/api") (encode @CounterRequest request)
    case response ^? Wreq.responseBody of
        Just body -> case decode @CounterResponse body of
            Just myResponse -> return myResponse
            Nothing -> throwIO (CounterError "Response decode error")
        Nothing -> throwIO (CounterError "network error")

evalCounterOnClient :: Host -> Port -> Counter a -> IO a
evalCounterOnClient host port (Add n) =
    callCounterApi host port (AddRequest n) >>= \case
        AddResponse v -> return v
        ResetResponse _ -> throwIO (CounterError "Unexpected response")
evalCounterOnClient host port Reset =
    callCounterApi host port ResetRequest >>= \case
        ResetResponse v -> return v
        AddResponse _ -> throwIO (CounterError "Unexpected response")

--------------------------------------------------------------------------------
-- server
evalCounterOnServer :: CounterState -> Counter a -> IO a
evalCounterOnServer state (Add n) = atomically $ do
    m <- readTVar (counter state)
    writeTVar (counter state) (m + n)
    return (m + n)
evalCounterOnServer state Reset = atomically $ writeTVar (counter state) 0

jsonApiRequestHandler :: CounterState -> CounterRequest -> IO CounterResponse
jsonApiRequestHandler state (AddRequest n) = AddResponse <$> evalCounterOnServer state (Add n)
jsonApiRequestHandler state ResetRequest = ResetResponse <$> evalCounterOnServer state Reset

counterServer :: Port -> IO ()
counterServer port = do
    state <- newCounterState 0
    jsonServer port (jsonApiRequestHandler state)

jsonServer :: (FromJSON request, ToJSON response) => Port -> (request -> IO response) -> IO ()
jsonServer (Port port) requestHandler = Scotty.scotty port $ do
    Scotty.post "/api" $ do
        reqBody <- Scotty.body
        case decode reqBody of
            Just request -> do
                response <- liftIO $ requestHandler $ request
                Scotty.json response
            Nothing -> Scotty.status badRequest400

--------------------------------------------------------------------------------
main :: IO ()
main = do
    let port = 4000
    let host = "localhost"
    let add n = evalCounterOnClient host port (Add n)
    let reset = evalCounterOnClient host port Reset

    _ <- forkIO $ counterServer port

    threadDelay 1000000
    print =<< add 1
    print =<< add 1
    print =<< reset
    print =<< add 1
    print =<< add 1
    print =<< add 1
    print =<< add 1


