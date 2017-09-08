{-# LANGUAGE OverloadedStrings #-}
-- src/Web/View/Main.hs

module Web.View.Main (loadMainTemplate, mainView) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Function          (on)
import           Data.List              (groupBy, sortBy)
import qualified Data.Text              as TXT
import qualified Data.Time.Format       as TM
import qualified Data.Time.LocalTime    as TM
import qualified Entity.User            as User
import qualified Entity.WeightRecord    as WRecord
import           Model.WeightRecord     (selectWRecord)
import           Text.Mustache
    ( Template
    , automaticCompile
    , object
    , substitute
    , (~>)
    )
import           Text.Mustache.Types    (Value)
import           Web.Core
    ( WRAction
    , WRConfig (wrcTplRoots)
    , WRState (wrstMainTemplate)
    , runSqlite
    , wrconUser
    )
import           Web.Spock              (getContext, getState, html)

-- src/Web/View/Main.hsに追記。
-- テンプレートの読み込み。
loadMainTemplate :: WRConfig -> IO Template
loadMainTemplate cfg = do
  compiled <- automaticCompile (wrcTplRoots cfg) "main.mustache"
  case compiled of
    Left err -> error (show err)
    Right template -> return template

-- ユーザ情報を変換して渡せるようにする。
userValue :: User.User -> WRAction Value
userValue u = return $ object ["id" ~> User.id u, "name" ~> User.name u]

-- 体重の履歴を変換して渡せるようにする。
weightRecordValue :: WRecord.WeightRecord -> WRAction Value
weightRecordValue wr = do
  ztime <- liftIO $ toZonedTime $ WRecord.time wr
  return $ object ["weight" ~> WRecord.weight wr, "time" ~> show ztime]

-- SQLite中のUTCからローカルタイムに変換する。
toZonedTime :: TM.LocalTime -> IO TM.ZonedTime
toZonedTime = TM.utcToLocalZonedTime . TM.localTimeToUTC TM.utc

-- グラフ表示の実装。
weightGraphValue :: [WRecord.WeightRecord] -> WRAction [Value]
weightGraphValue wrs = do
    flatWrs <- liftIO $ mapM flat wrs
    let wrss = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ flatWrs
    return $ map groupToValue wrss
  where
    -- WeightRecord型を日付と体重のタプルにして扱いやすくする。
    flat wr = do
      ztime <- toZonedTime $ WRecord.time wr
      let ztimeStr = TM.formatTime TM.defaultTimeLocale "%m/%d" ztime
          w = WRecord.weight wr
      return (ztimeStr, w)
    -- グループをmustacheで扱うための値にする。
    groupToValue gr =
      let dt = head $ map fst gr -- groupByの仕様からgrは[]ではなく、headが使える
          wt = avg $ map snd gr
          avg xs = sum xs / fromIntegral (length xs)
      in object ["day" ~> dt, "weight" ~> wt]

-- src/Web/View/Main.hsに追記する。
-- メッセージを受け取って
mainView :: Maybe TXT.Text -> WRAction a
mainView mMes = do
    Just user <- wrconUser <$> getContext
    uv <- userValue user -- ユーザ情報
    rs <- runSqlite $ selectWRecord (User.id user) -- SQLiteの情報
    rvs <- mapM weightRecordValue rs
    wgv <- weightGraphValue rs
    tpl <- wrstMainTemplate <$> getState
    let v =
            object $
            appendMessage
                mMes
                ["user" ~> uv, "records" ~> rvs, "graphs" ~> wgv]
    html $ substitute tpl v
  where
    appendMessage (Just mes) ps = "message" ~> mes : ps
    appendMessage Nothing ps = ps
