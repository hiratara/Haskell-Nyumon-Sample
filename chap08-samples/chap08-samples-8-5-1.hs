{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async
import qualified Data.ByteString.Lazy.Char8 as C8
import Network.HTTP.Simple -- HTTPリクエストを投げる

main :: IO ()
main = do
  a1 <- async $ getUrl "http://httpbin.org/get"
  a2 <- async $ getUrl "http://httpbin.org/get"
  result1 <- wait a1
  result2 <- wait a2
  C8.putStrLn result1
  C8.putStrLn result2

getUrl :: Request -> IO C8.ByteString
getUrl url = do
  response <- httpLBS url
  return $ getResponseBody response
