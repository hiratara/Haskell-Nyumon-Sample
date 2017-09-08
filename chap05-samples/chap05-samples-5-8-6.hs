module Main(main) where

import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Except (Except, runExcept, throwError, catchError)

mtlSample :: Either String ((), String)
mtlSample = runExcept $ runWriterT $ do
  tell "Start\n"
  (`catchError` handler) $ do
    tell "In the block\n"
    _ <- throwError "some exception"
    tell "Never reach here\n"
  tell "End\n"
  where
    handler :: String -> WriterT String (Except String) ()
    handler e = tell $ "Caught the exception: " ++ e ++ "\n"

main :: IO ()
main = print mtlSample
