module Main(main) where

main :: IO ()
main =
  getLine >>= \x -> -- 変数 x に代入してるように見える
  getLine >>= \y -> -- 変数 y に代入してるように見える
  putStrLn ("1つ目の入力 : " ++ x) >>
  putStrLn ("2つ目の入力 : " ++ y)
