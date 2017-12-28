{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Entity.WeightRecord where

import Entity.TH (defineTableFromName)

-- Template Haskellを用いてデータ型を定義する。
defineTableFromName "weight_record" [("FLOAT", [t|Double|]), ("INTEGER", [t|Int|])]
