module Main(main) where

main :: IO ()
main = do
  x <- getLine
  putStrLn $ "1つ目の入力 : " ++ x
  getLine >>= return . ("2つ目の入力 : " ++) >>= putStrLn
