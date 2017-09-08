module Main(main) where

import qualified Data.ByteString as B
import Data.Bits

main :: IO ()
main = do
  s <- B.readFile "sample"
  B.writeFile "sample" . B.map complement $ s
