module Main(main) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (lift)

data Env = Env { envX :: !Integer, envY :: !Integer }

sumEnv :: ReaderT Env IO Integer
sumEnv = do
  x <- asks envX
  y <- asks envY
  return (x + y)

sumEnvIO :: ReaderT Env IO Integer
sumEnvIO = do
  x <- asks envX
  lift $ putStrLn ("x=" ++ show x)
  y <- asks envY
  lift $ putStrLn ("y=" ++ show y)
  return (x + y)


main :: IO ()
main = do
  print =<< runReaderT sumEnv (Env 10 20)
  print =<< runReaderT sumEnvIO (Env 10 20)
