module Main(main) where

import Data.List.Split (splitOn)

data YMD = YMD Int Int Int deriving Show
data HMS = HMS Int Int Int deriving Show

-- 日付形式 1987/07/23
parseYMD :: String -> Maybe YMD
parseYMD = listToYmd . splitOn "/"
  where
    listToYmd :: [String] -> Maybe YMD
    listToYmd (y:m:d:_) = Just $ YMD (read y) (read m) (read d)
    listToYmd _ = Nothing

-- 日時形式 1987/07/23 15:00:00
parseHMS :: String -> Maybe HMS
parseHMS = listToHms . splitOn ":"
  where
    listToHms :: [String] -> Maybe HMS
    listToHms (h:m:s:_) = Just $ HMS (read h) (read m) (read s)
    listToHms _ = Nothing

parseDateTime :: String -> Maybe (YMD, HMS)
parseDateTime = listToDateTime . splitOn " "
  where
    listToDateTime :: [String] -> Maybe (YMD, HMS)
    listToDateTime (d:t:_) = (,) <$> parseYMD d <*> parseHMS t
    listToDateTime _ = Nothing

main :: IO ()
main = do
  print $ parseDateTime "1987/07/23 15:00:00"
