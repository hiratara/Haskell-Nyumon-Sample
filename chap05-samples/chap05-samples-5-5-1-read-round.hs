module Main(main) where

import Control.Monad.Trans.Reader (Reader, ask, runReader)

main :: IO ()
main = print $ runReader readRound 1.5013232

readRound :: Reader Double Int
readRound = do
  x <- ask
  return $ round x
