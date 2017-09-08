module Main (main) where

main :: IO ()
main = foldr f (return ()) $ map fizzbuzz [1 .. 20]
  where
    fizzbuzz n | n `mod` 15 == 0 = "FizzBuzz"
               | n `mod` 3  == 0 = "Fizz"
               | n `mod` 5  == 0 = "Buzz"
               | otherwise       = show n
    f str act = do putStrLn str
                   act
