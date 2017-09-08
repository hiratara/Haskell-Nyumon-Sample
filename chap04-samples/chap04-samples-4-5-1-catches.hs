{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Control.Exception

someOperation :: IO ()
someOperation = throwIO $ userError "本来は何らかの処理を実装する"

main :: IO ()
main =
  someOperation
    `catches`
  [ Handler $ \(e :: ArithException) ->
      putStrLn $ "Catch ArithException: " ++ displayException e
  , Handler $ \(e :: SomeException) ->
      putStrLn $ "Catch SomeException: " ++ displayException e
  ]
