{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Attoparsec.Text

main :: IO ()
main = do
  print (parse decimal "1000" :: Result Int)
  print (parse decimal "1000" `feed` "" :: Result Int)
