module Main(main) where

import Control.Exception

main :: IO ()
main =
  (readFile "dummyFileName" >>= putStrLn)
    `catch`
  (\e ->
    putStrLn $ "readFile failure!!! : " ++ displayException (e :: SomeException))
