-- src/Web/Core.hs
module Web.Core
    (WRState(WRState, wrstMainTemplate, wrstStartTemplate),
     WRContext(WRContext, wrconUser), emptyContext, WRConnection,
     WRSession(WRSession, wrsesUser), emptySession, WRApp, WRAction,
     runSqlite, WRConfig(WRConfig, wrcDBPath, wrcTplRoots, wrcPort))
  where

import           Control.Monad.IO.Class (liftIO)
import           Database.HDBC.Sqlite3  (Connection)
import qualified Entity.User            as User
import           Text.Mustache          (Template)
import           Web.Spock              (SpockActionCtx, SpockCtxM, runQuery)

data WRConfig = WRConfig
  { wrcDBPath   :: !FilePath
  , wrcTplRoots :: ![FilePath]
  , wrcPort     :: !Int
  }

data WRState = WRState
  { wrstStartTemplate :: !Template
  , wrstMainTemplate  :: !Template
  }

-- ルーティングに提供する情報。接頭辞のWRはアプリケーション名に由来。
newtype WRContext = WRContext
  { wrconUser :: Maybe User.User
  }

emptyContext :: WRContext
emptyContext = WRContext Nothing

-- コネクションプールの型。
type WRConnection = Connection

-- HTTPセッションのデータ型。
newtype WRSession = WRSession
  { wrsesUser :: Maybe User.User
  }

emptySession :: WRSession
emptySession = WRSession Nothing

type WRApp ctx = SpockCtxM ctx WRConnection WRSession WRState

type WRAction = SpockActionCtx WRContext WRConnection WRSession WRState

-- モデル呼び出し（SQLite実行）の関数。
runSqlite :: (Connection -> IO m) -> WRAction m
runSqlite f = runQuery $ liftIO . f
