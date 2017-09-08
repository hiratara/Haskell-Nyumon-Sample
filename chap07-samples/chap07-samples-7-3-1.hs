{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.ByteString.Char8

cat :: ByteString
cat = "Meow!"

dog :: ByteString
dog = "Bowwow!"

main :: IO ()
main = do
  Data.ByteString.Char8.putStr cat
  Data.ByteString.Char8.putStr "\n"
  Data.ByteString.Char8.putStr dog
  Data.ByteString.Char8.putStr "\n"
