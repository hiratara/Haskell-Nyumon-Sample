module Main (main) where

import MyApp.SomeModule

main :: IO ()
main = do
  helloMyApp "Haskell"
  byeMyApp "Others"
