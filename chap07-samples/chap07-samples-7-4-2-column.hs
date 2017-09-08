module Main(main) where

import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  animals <- VM.new 2 -- 2要素の配列を作成
  VM.write animals 0 "Dog"
  VM.write animals 1 "Cat"

  -- IOアクションを直接引数にしているため、エラーとなる
  -- putStrLn $ VM.read animals 1 -- Cat と表示したい
