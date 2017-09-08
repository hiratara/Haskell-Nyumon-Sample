module Main(main) where

-- exSafeDiv.hs
import qualified Control.Monad.Trans.Except as EX

safeDiv :: Integer -> Integer -> EX.Except String Integer
safeDiv k n | n == 0    = EX.throwE $ "Illegal division by zero. k:" ++ show k
            | otherwise = return (k `div` n)

calc :: Integer -> Either String Integer
calc n = EX.runExcept $ do
  EX.catchE (do x <- 100 `safeDiv` n
                y <- 100 `safeDiv` (x - 1)
                return y)
            (\_ -> return 0)

main :: IO ()
main = do
  print $ calc 50
  print $ calc 0
