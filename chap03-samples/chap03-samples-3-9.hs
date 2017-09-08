{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Data.List (intercalate)

class Greeting a where
  name  :: a -> String
  hello :: a -> String
  hello _ = "..." -- hello関数のデフォルトの実装
  bye   :: a -> String
  bye   _ = "..." -- bye関数のデフォルトの実装

class Greeting a => Laughing a where
  laugh :: a -> String

data Human = Human String deriving Show

instance Greeting Human where
  name  (Human n) = n
  hello h         = "Hi, I'm " ++ name h ++ "."
  bye   _         = "See you."

instance Laughing Human where
  laugh _ = "Zehahahah...!!"

sayHello :: Greeting a => a -> IO ()
sayHello x = putStrLn (hello x)

leaveWithLaugh :: Laughing a => a -> IO ()
leaveWithLaugh x = do
  putStrLn (bye x)
  putStrLn (laugh x)

-- `f` で得られる文字列を改行で連結する関数の定義
-- intercalateは第一引数に連結に使う文字列、第二引数に連結したい文字列のリストをとる
liftGreet :: (a -> String) -> ([a] -> String)
liftGreet f = intercalate "\n" . map f

instance Greeting a => Greeting [a] where
  name  = liftGreet name
  hello = liftGreet hello
  bye   = liftGreet bye

class Breeding a where
  breed :: String -> a

instance Breeding Human where
  breed = Human -- このHumanはコンストラクタ

clone :: (Breeding a, Greeting a) => a -> a
clone x = breed (name x) `asTypeOf` x

newtype BString = NewBString { unBString :: String } deriving Show

instance Breeding BString where breed = NewBString

main :: IO ()
main = do
  sayHello (Human "takashi")

  leaveWithLaugh (Human "takashi")

  sayHello [Human "atsuhiko", Human "shingo"]

  let baby = breed "takeshi"
  print $ hello (baby :: Human)

  print $ hello $ clone (Human "takeshi")

  print (breed "a raw string" :: BString)
