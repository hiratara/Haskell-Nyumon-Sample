module Main(main) where

import System.Directory

main :: IO ()
main =
  getPermissions "sample.txt"
    >>= setPermissions "sample.txt" . toReadAndWritable

toReadAndWritable :: Permissions -> Permissions
toReadAndWritable p = p { readable = True, writable = True }
