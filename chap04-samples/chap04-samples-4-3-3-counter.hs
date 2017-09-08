module Main(main) where

main :: IO ()
main = do
  xs <- getContents >>= return . lines
  counter 0 xs

counter :: Int -> [String] -> IO ()
counter _ [] = return ()
counter i ("up":xs) = print (i + 1) >> counter (i + 1) xs
counter i ("down":xs) = print (i - 1) >> counter (i - 1) xs
counter i (_:xs) = counter i xs
