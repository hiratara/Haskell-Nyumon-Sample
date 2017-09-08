module Main(main) where

main1 :: IO ()
main1 = readFile "sample.txt" >>= putStrLn -- 遅延I/Oが有効

main2 :: IO ()
main2 = readFile "sample.txt" >>= putStrLn . reverse -- 遅延I/Oに意味がない

main3 :: IO ()
main3 = do
  val <- readFile "sample.txt"
  putStrLn $ take 5 val
  writeFile "sample.txt" "Hello, Lazy IO!"

main :: IO ()
main = do
  main1
  main2
  main3
