module Main(main) where

import Pipes
import qualified Pipes.Prelude as P

hello :: Pipe String String IO r
hello = do
  P.mapM $ \s -> do
    putStrLn $ "input : " ++ s
    return $ "Hello, " ++ s

main :: IO ()
main = runEffect $ P.stdinLn >-> hello >-> P.stdoutLn
