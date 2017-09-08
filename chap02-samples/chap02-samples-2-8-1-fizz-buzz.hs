module Main (main) where
import Control.Monad (when)

main :: IO ()
main = loop 0
  where
    loop n | n <= 20   = do
               when (n `mod` 3 /= 0 && n `mod` 5 /= 0) (putStr $ show n)
               when (n `mod` 3 == 0) (putStr "Fizz")
               when (n `mod` 5 == 0) (putStr "Buzz")
               putStrLn ""
               loop (n + 1)
           | otherwise = return ()
