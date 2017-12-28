-- src/Entity/User.hs
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Entity.User where

import Entity.TH (defineTableFromName)

-- Template Haskellを用いてデータ型を定義する。
defineTableFromName "user" [("INTEGER", [t|Int|])]
