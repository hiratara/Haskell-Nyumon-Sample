{-# LANGUAGE OverloadedStrings #-}
-- src/Web/WeightRecorder.hs

module Web.WeightRecorder
       (runWeightRecorder, weightRecorderMiddleware, WRConfig(..)) where

import           Data.Pool             (Pool, createPool)
import           Database.HDBC         (IConnection (disconnect))
import           Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import qualified Network.Wai           as WAI
import           Web.Action.Login      (loginAction)
import           Web.Action.NewRecord  (newRecordAction)
import           Web.Action.Register   (registerAction)
import           Web.Core
    ( WRAction
    , WRApp
    , WRConfig (..)
    , WRContext (wrconUser)
    , WRSession (wrsesUser)
    , WRState (WRState, wrstMainTemplate, wrstStartTemplate)
    , emptyContext
    , emptySession
    )
import           Web.Spock
    ( get
    , getContext
    , post
    , prehook
    , readSession
    , root
    , runSpock
    , spock
    )
import           Web.Spock.Config
    ( PoolOrConn (PCPool)
    , defaultSpockCfg
    )
import           Web.View.Main         (loadMainTemplate, mainView)
import           Web.View.Start        (loadStartTemplate, startView)

-- src/Web/WeightRecorder.hsに追記。
authHook :: WRAction WRContext
authHook = do
  ctx <- getContext
  mUser <- fmap wrsesUser readSession
  case mUser of
    Nothing -> startView Nothing
    Just user ->
      return $
      ctx
      { wrconUser = Just user
      }

-- src/Web/WeightRecorder.hsに追記。
-- ルーティング定義。
spockApp :: WRApp () ()
spockApp =
  prehook (return emptyContext) $
  do prehook authHook $
       do get root $ mainView Nothing
          post "new_record" newRecordAction
     post "register" registerAction
     post "login" loginAction

-- src/Web/WeightRecorder.hsに追記。
weightRecorderMiddleware :: WRConfig -> IO WAI.Middleware
weightRecorderMiddleware cfg = do
  -- テンプレートの読み込み。
  starttpl <- loadStartTemplate cfg
  maintpl <- loadMainTemplate cfg
  -- 状態を作成。
  let state =
        WRState
        { wrstStartTemplate = starttpl
        , wrstMainTemplate = maintpl
        }
  pool <- sqlitePool $ wrcDBPath cfg
  -- セッションと状態を渡して設定を作成。
  spCfg <- defaultSpockCfg emptySession (PCPool pool) state
  spock spCfg spockApp -- 生成。

-- src/Web/WeightRecorder.hsに追記。
sqlitePool :: FilePath -> IO (Pool Connection)
sqlitePool dbpath = createPool (connectSqlite3 dbpath) disconnect 1 60 5

-- src/Web/WeightRecorder.hsに追記。
runWeightRecorder :: WRConfig -> IO ()
runWeightRecorder cfg = runSpock (wrcPort cfg) (weightRecorderMiddleware cfg)
