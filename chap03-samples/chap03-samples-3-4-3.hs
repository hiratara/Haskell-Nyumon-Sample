module Main (main) where

data CmdOption = COptInt Integer
               | COptBool Bool
               | COptStr String
               deriving Show
opt1, opt2 :: CmdOption
opt1 = COptInt 120
opt2 = COptStr "0x78"

coptToInt :: CmdOption -> Int
coptToInt (COptInt  n    ) = fromIntegral n
coptToInt (COptStr  x    ) = read x
coptToInt (COptBool True ) = 1
coptToInt (COptBool False) = 0

main :: IO ()
main = do
  print $ coptToInt opt1
  print $ coptToInt opt2
