module Main(main) where

-- mSafeDiv.hs
safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv k n | n == 0    = Nothing
            | otherwise = Just (k `div` n)

calc :: Integer -> Maybe Integer
calc n = do
  x <- 100 `safeDiv` n       -- 100を引数で割る。
  y <- 100 `safeDiv` (x - 1) -- 100をx - 1で割る。
  return y

main :: IO ()
main = do
  print $ calc 50
  print $ calc 0
