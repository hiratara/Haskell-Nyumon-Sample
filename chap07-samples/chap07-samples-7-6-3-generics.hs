{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main(main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Generic)

instance ToJSON Person
instance FromJSON Person

taro :: Person
taro = Person
  { name = "Taro"
  , age = 30
  }

hanako :: B.ByteString
hanako = "{\"name\":\"Hanako\",\"age\":25}"

jiro :: B.ByteString
jiro = "{\"onamae\":\"Jiro\",\"nenrei\":30}"

main :: IO ()
main = do
  B.putStrLn . encode $ taro
  print (decode hanako :: Maybe Person)
  print (decode jiro :: Maybe Person)

  print (eitherDecode hanako :: Either String Person)
  print (eitherDecode jiro :: Either String Person)
