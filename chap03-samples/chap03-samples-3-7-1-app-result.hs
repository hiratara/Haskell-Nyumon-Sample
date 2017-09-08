module Main (main) where

data AppErr = NewAppErr deriving Show
type AppResult a = Either AppErr a

safeHead :: [a] -> AppResult a
safeHead [] = Left  NewAppErr
safeHead xs = Right (head xs)

main :: IO ()
main = do
  print $ safeHead ([1,2,3] :: [Int])
  print $ safeHead ([] :: [Int])
