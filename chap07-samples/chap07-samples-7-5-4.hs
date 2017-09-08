{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Attoparsec.Text

data YMD = YMD Int Int Int deriving Show
data HMS = HMS Int Int Int deriving Show

ymdParser :: Parser YMD
ymdParser = YMD
  <$> (countRead 4 digit <?> "Year") <* (char '/' <?> "Delim Y/M")
  <*> (countRead 2 digit <?> "Month") <* (char '/' <?> "Delim M/D")
  <*> (countRead 2 digit <?> "Day")

hmsParser :: Parser HMS
hmsParser = HMS
  <$> (countRead 2 digit <?> "Hour") <* (char ':' <?> "Delim H:M")
  <*> (countRead 2 digit <?> "Minute") <* (char ':' <?> "Delim M:S")
  <*> (countRead 2 digit <?> "Second")

dateTimeParser :: Parser (YMD, HMS)
dateTimeParser = (,)
  <$> (ymdParser <?> "YMD")
  <*  (char ' ' <?> "space")
  <*> (hmsParser <?> "HMS")

countRead :: Read a => Int -> Parser Char -> Parser a
countRead i = fmap read . count i

main :: IO ()
main = do
  print $ parse (dateTimeParser <* endOfInput) "2018/08/21hoge 12:00:00" `feed` ""
  print $ parse (dateTimeParser <* endOfInput) "2018/08/21 12:00.00" `feed` ""
