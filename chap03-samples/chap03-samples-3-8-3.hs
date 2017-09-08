module Main (main) where

class Greeting a where
  name  :: a -> String
  hello :: a -> String
  hello _ = "..." -- hello関数のデフォルトの実装
  bye   :: a -> String
  bye   _ = "..." -- bye関数のデフォルトの実装

data Human = Human String deriving Show

instance Greeting Human where
  name  (Human n) = n
  hello h         = "Hi, I'm " ++ name h ++ "."
  bye   _         = "See you."

data Dog = Dog deriving Show

instance Greeting Dog where
  name  _ = "a dog"
  hello _ = "Bark!"

data Cat = Cat deriving Show

instance Greeting Cat where
  name  _ = "a cat"
  bye   _ = "Meow..."

main :: IO ()
main = do
  print $ hello (Human "takeshi")
  print $ hello Dog
  print $ hello Cat
  print $ bye (Human "takashi")
  print $ bye Dog
  print $ bye Cat
