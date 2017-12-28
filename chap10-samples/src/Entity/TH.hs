{-# LANGUAGE TemplateHaskell, TypeApplications #-}

module Entity.TH (defineTableFromName) where

import Control.Exception.Safe       (tryIO)
import Database.HDBC.Query.TH       (defineTableFromDB)
import Database.HDBC.Schema.Driver  (typeMap, TypeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3        (connectSqlite3)
import Language.Haskell.TH          (Dec, Q, runIO)
import System.Environment           (getEnv)

defineTableFromName :: String -> TypeMap -> Q [Dec]
defineTableFromName name typemap = do
  path' <- runIO $ tryIO $ getEnv "WEIGHT_DB_PATH"
  let path = either (const "weight.db") id path'
  defineTableFromDB (connectSqlite3 path)
    (driverSQLite3 { typeMap = typemap })
    "main" name [''Show]
