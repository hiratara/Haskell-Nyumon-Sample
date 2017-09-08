module Main(main) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (evalStateT, get, modify)
import Control.Monad.Trans.Except (runExceptT, throwE)

main :: IO ()
main = do
  -- I/Oアクションが可能なdo式
  result <- (`evalStateT` 0) $ runExceptT $ loop
  case result of
    Right _ -> return ()
    Left  e -> putStrLn e

  where
    loop = do
      -- I/Oアクション、状態操作、例外処理が可能
      i <- st $ get
      unless (i < (3 :: Int)) $ throwE "Too much failure"

      op <- io $ getLine
      if op == "end" then
        return ()
      else do
        st $ modify (+ 1)
        loop

    io = lift . lift
    st = lift
