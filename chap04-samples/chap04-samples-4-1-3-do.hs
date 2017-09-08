module Main(main) where

main :: IO ()
main = do
  x <- getLine -- getLineの返り値を束縛。
  y <- getLine -- getLineの返り値を束縛。
  putStrLn $ "1つ目の入力 : " ++ x
  putStrLn $ "2つ目の入力 : " ++ y
