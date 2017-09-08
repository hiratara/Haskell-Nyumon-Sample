{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Control.Exception

main :: IO ()
main =
  (readFile "dummyFileName" >>= putStrLn)
    `catch`
  (\(e :: SomeException) ->
    putStrLn $ "readFile failure!!! : " ++ displayException e)
    `finally`
  (putStrLn "finalization!!!")
