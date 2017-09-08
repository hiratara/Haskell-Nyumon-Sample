module Main(main) where

import Control.Monad
import Control.Applicative

assocs :: [(String, Int)]
assocs = [("hiratara", 39), ("shu1", 0), ("masaharu", 32)]

main :: IO ()
main = do
  print $ lookup "hiratara" assocs
  print $ lookup "homma" assocs

  print $ lookup "homma" assocs <|> lookup "hiratara" assocs

  print $ do age <- lookup "homma" assocs <|> lookup "hiratara" assocs
             guard $ age < 20
             return age
