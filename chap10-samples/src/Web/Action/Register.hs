{-# LANGUAGE OverloadedStrings #-}
-- src/Web/Action/Register.hs

module Web.Action.Register (registerAction) where

import Control.Monad  (when)
import Model.User     (NewUser (NewUser), insertUser)
import Web.Core       (WRAction, runSqlite)
import Web.Spock      (param')
import Web.View.Start (startView)

registerAction :: WRAction a
registerAction = do
  name <- param' "name" -- paramで値を取り出す。
  password <- param' "password"
  when (null name || null password) $ startView (Just "入力されてない項目があります") -- 未入力項目があれば入力を促す。
  n <- runSqlite $ insertUser (NewUser name password)
  when (n <= 0) $ startView (Just "登録に失敗しました") -- INSERT失敗時に登録失敗を返す。
  startView (Just "登録しました。ログインしてください。")
