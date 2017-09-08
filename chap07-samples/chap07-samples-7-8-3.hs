{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

-- データ構造の定義
data User = User
  { _userName :: String
  , _userAge :: Int
  , userPassword :: String -- _がないので生成されない。
  } deriving Show

makeLenses ''User -- ''はTemplateHaskellを使う箇所

userPass :: Lens User User String String
userPass = lens userPassword (\user password -> user { userPassword = password } )

main :: IO ()
main = do
  let user = User
           { _userName = "Taro"
           , _userAge = 25
           , userPassword = "12345"
           }
  -- USer型に対しアクセサが生成される
  -- userName :: Functor f => Lens User User String String
  -- userAge :: Functor f => Lens User User Int Int
  print (user^.userName)        -- "Taro"
  print (user^.userAge)         -- 25
  print (user&userName.~"Jiro") -- User {_userName = "Jiro", _userAge = 25, userPassword = "12345"}

  print (user^.userPass) -- "12345"
  print (user&userPass.~"new-password") -- User {_userName = "Taro", _userAge = 25, userPassword = "new-password"}
