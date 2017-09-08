module Main (main) where

-- Main.hsに書き、`stack runghc -- Main.hs`で実行。
-- Int型の第一引数、Int型の第二引数、Int型のリストの返り値。
intToArray :: Int -> Int -> [Int]
-- リストからl分取り出す。
intToArray i l = take l (repeat i)

main :: IO ()
main = do
  -- 型に合致するため実行できる。
  print (intToArray 3 4) -- [3,3,3,3]
  -- 第二引数の型が一致しないためコンパイルエラーが発生する。
  -- print (intToArray 5000 12.04)
  -- 第一引数（i :: Int）の型が一致しないためコンパイルエラーが発生する。repeat自体は引数にChar型も許容するがintToArrayの型を明示しているためここでは利用できない。
  -- print (intToArray 'a' 10)
