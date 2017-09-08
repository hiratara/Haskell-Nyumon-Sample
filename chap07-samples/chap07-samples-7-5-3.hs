{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Attoparsec.Text

data YMD = YMD Int Int Int deriving Show
data HMS = HMS Int Int Int deriving Show

ymdParser :: Parser YMD
ymdParser = YMD
  <$> countRead 4 digit <* char '/'
  <*> countRead 2 digit <* char '/'
  <*> countRead 2 digit

hmsParser :: Parser HMS
hmsParser = HMS
  <$> countRead 2 digit <* char ':'
  <*> countRead 2 digit <* char ':'
  <*> countRead 2 digit

dateTimeParser :: Parser (YMD, HMS)
dateTimeParser = (,) <$> ymdParser <* char ' ' <*> hmsParser

countRead :: Read a => Int -> Parser Char -> Parser a
countRead i = fmap read . count i

main :: IO ()
main = do
  print $ parse dateTimeParser "2018/08/21hoge 12:00:00" `feed` ""
  print $ parse dateTimeParser "2018/08/21 12:00:00hoge" `feed` ""

  print $ parse (dateTimeParser <* endOfInput) "2018/08/21 12:00:00hoge" `feed` ""
