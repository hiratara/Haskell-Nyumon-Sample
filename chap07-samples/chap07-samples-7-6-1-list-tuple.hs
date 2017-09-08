{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
  B.putStrLn $ encode (["Taro", "Jiro", "Hanako"] :: [String])
  B.putStrLn $ encode ([10, 20, 30] :: [Int])
  B.putStrLn $ encode (("Hello", 100) :: (String, Int))
  print (decode "[\"Taro\", \"Jiro\", \"Hanako\"]" :: Maybe [String])
  print (decode "[10, 20, 30]" :: Maybe [Int])
  print (decode "[777 , \"Haskell\"]" :: Maybe (Int, String))
