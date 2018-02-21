{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, DataKinds, DeriveGeneric #-}

module Entity.WeightRecord where

import Database.HDBC.Query.TH       (defineTableFromDB)
import Database.HDBC.Schema.Driver  (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3        (connectSqlite3)
import GHC.Generics                 (Generic)

defineTableFromDB (connectSqlite3 "weight.db")
  (driverSQLite3 { typeMap = [("FLOAT", [t|Double|]), ("INTEGER", [t|Int|])] })
  "main" "weight_record"
  [''Show, ''Generic]
