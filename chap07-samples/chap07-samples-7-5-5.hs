{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Attoparsec.Text
import Control.Applicative

-- 誤った実装例。無限再帰する
-- data Expr = Add Expr Expr | Val Double deriving (Show, Read, Eq)
-- 
-- addParser :: Parser Expr
-- addParser = do
--   left <- exprParser
--   char '+'
--   right <- exprParser
--   return $ Add left right
-- 
-- exprParser :: Parser Expr
-- exprParser = addParser <|> (double >>= return . Val)

data Term = Add Expr deriving Show
data Expr = ExTerm Double Term | ExEnd Double deriving Show

termParser :: Parser Term
termParser = addParser
  where
    addParser :: Parser Term
    addParser = Add <$ char '+' <*> exprParser

exprParser :: Parser Expr
exprParser = ExTerm <$> double <*> termParser <|> ExEnd <$> double

main :: IO ()
main = do
--  print $ parse (exprParser' <* endOfInput) "1+2+3" `feed` ""

  print $ parse (exprParser <* endOfInput) "1+2+3" `feed` ""
