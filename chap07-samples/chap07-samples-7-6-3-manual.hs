{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy.Char8 as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving Show

instance ToJSON Person where
  toJSON (Person n a) =
    object ["name" .= n, "age" .= a]
instance FromJSON Person where
  parseJSON (Object v) = Person
    <$> v .: "name"
    <*> v .: "age"
  parseJSON i = typeMismatch "Person" i

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
