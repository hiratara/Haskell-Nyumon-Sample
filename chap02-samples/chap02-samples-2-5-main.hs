-- main.hs
module Main (main) where
import System.Environment

main :: IO ()
main = do
  let title = "Current User:"
  user <- getEnv "USER" -- Windowsの場合はUSERNAME
  putStrLn title
  putStrLn user
