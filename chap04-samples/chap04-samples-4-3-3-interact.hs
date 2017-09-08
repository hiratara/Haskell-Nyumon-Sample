module Main(main) where

import Data.Char (toUpper)

main :: IO ()
main = interact $ map toUpper
