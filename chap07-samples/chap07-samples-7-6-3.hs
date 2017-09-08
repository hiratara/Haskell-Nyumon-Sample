{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as B

data Human = Human
  { name :: String
  , age :: Int
  } deriving Show

deriveJSON defaultOptions ''Human

data Department = Department
  { departmentName :: String
  , coworkers :: [Human]
  } deriving Show

deriveJSON (defaultOptions
  { fieldLabelModifier = \s -> case s of
      "departmentName" -> "name"
      t -> t
  } ) ''Department -- TemplateHaskell由来

taro :: Human
taro = Human { name = "Taro" , age = 30 }

saburo :: Human
saburo = Human { name = "Saburo" , age = 31 }

shiro :: Human
shiro = Human { name = "Shiro" , age = 31 }

matsuko :: Human
matsuko = Human { name = "Matsuko" , age = 26 }

nameList :: [Department]
nameList =
  [ Department
    { departmentName = "General Affairs"
    , coworkers =
      [ taro
      , matsuko
      ]
    }
  , Department
    { departmentName = "Development"
    , coworkers =
      [ saburo
      , shiro
      ]
    }
  ]

main :: IO ()
main = do
  B.putStrLn $ encode $ nameList
