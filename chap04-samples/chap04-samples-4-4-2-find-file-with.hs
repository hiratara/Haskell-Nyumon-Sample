module Main(main) where

import System.Directory

main :: IO ()
main = do
  current <- getCurrentDirectory
  let checkWritable filePath = getPermissions filePath >>= return . writable
  findFilesWith
    checkWritable [current ++ "/.." , current] "target.txt" >>= print
