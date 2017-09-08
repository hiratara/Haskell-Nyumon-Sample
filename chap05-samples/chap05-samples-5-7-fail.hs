{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main(main) where

import System.IO.Error (tryIOError)

monadHead, monadHead' :: Monad m => [a] -> m a
monadHead xs = do
  (x:_) <- return xs
  return x

monadHead' xs = do
  let (x:_) = xs
  return x

main :: IO ()
main = do
  print (monadHead [] :: Maybe Int)
  print (monadHead [] :: [Int])

  -- 評価結果はボトムとなる
  -- print (monadHead [] :: Either String Int)

  result <- tryIOError $ monadHead [] :: IO (Either IOError Int)
  print result

  -- 評価結果はボトムとなる
  -- print (monadHead' [] :: Maybe Int)
