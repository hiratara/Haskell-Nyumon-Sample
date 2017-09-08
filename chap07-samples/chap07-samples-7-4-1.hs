module Main(main) where

import qualified Data.Vector as V

main :: IO ()
main = do
  let animals = V.fromList ["Dog", "Pig", "Cat", "Fox", "Mouse", "Cow", "Horse"]
  -- 配列内のすべての文字列の文字数を数え上げて足す
  print . V.sum . V.map length $ animals
