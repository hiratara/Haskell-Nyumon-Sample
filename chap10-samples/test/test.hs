{-# LANGUAGE OverloadedStrings #-}
-- test/test.hs

module Main (main) where

import           Control.Monad             (void)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Network.HTTP.Types        as HTTP
import qualified Network.Wai               as WAI
import qualified Network.Wai.Test          as WT
import           Paths_weight_recorder     (getDataDir)
import           System.FilePath           ((</>))
import           System.IO.Temp            (withSystemTempFile)
import           System.Process            (callCommand)
-- HUnit関連。
import           Test.HUnit
    ( Assertion
    , Test (TestCase)
    , runTestTT
    )
-- 作成したアプリケーション関連。
import           Web.Spock                 (spockAsApp)
import           Web.WeightRecorder
    ( WRConfig (WRConfig, wrcDBPath, wrcPort, wrcTplRoots)
    , weightRecorderMiddleware
    )

-- src/test/test.hsに続けて書く...。
main :: IO ()
main = void $ runTestTT (TestCase testWeightRecorder) -- テスト実行

-- SQLの作成。data/schema.sqlからDBを作成する。
sqlFile :: IO FilePath
sqlFile = do
    datadir <- getDataDir
    return $ datadir </> "data" </> "schema.sql"

-- テスト。
testWeightRecorder :: Assertion
testWeightRecorder =
    withSystemTempFile "test.db" $
    \path _ ->
         do sql <- sqlFile
            callCommand $ "sqlite3 " ++ path ++ " < " ++ sql
            datadir <- getDataDir
            let cfg =
                    WRConfig
                    { wrcDBPath = path
                    , wrcTplRoots = [datadir </> "templates"]
                    , wrcPort = 9999 -- Won't use this value while testing
                    }
                m = weightRecorderMiddleware cfg
            WT.runSession basic =<< spockAsApp m

-- src/test/test.hsに追記...。
get :: BS.ByteString -> (WT.SResponse -> WT.Session ()) -> WT.Session ()
get url = request HTTP.methodGet [] url ""

post
    :: BS.ByteString
    -> LBS.ByteString
    -> (WT.SResponse -> WT.Session ())
    -> WT.Session ()
post url body = request HTTP.methodPost postHeaders url body
  where
    postHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")]

-- WAIアプリケーションとのやり取りのラッパー
request
    :: HTTP.Method
    -> [HTTP.Header]
    -> BS.ByteString
    -> LBS.ByteString
    -> (WT.SResponse -> WT.Session ())
    -> WT.Session ()
request meth hs url body cb = do
    let req =
            WT.defaultRequest
            { WAI.requestMethod = meth
            , WAI.requestHeaders = hs
            } `WT.setPath`
            url
    res <- WT.srequest (WT.SRequest req body)
    case lookup HTTP.hLocation (WT.simpleHeaders res) of
        Nothing -> cb res
        Just url' -> get url' cb

-- src/test/test.hsに追記...。
-- 表示確認、入力表示確認を定義。

basic :: WT.Session ()
basic = do
    get "/" $
        \res ->
             do bodyContains "ユーザ登録" res -- 画面表示内容を取得して確認。
                bodyContains "ログイン" res
    post "/register" "name=hoge&password=hage" $ bodyContains "登録しました"  -- 入力と表示を確認。
    post "/login" "name=hoge&password=hage" $ bodyContains "体重の入力"
    post "/new_record" "weight=60.1" $ bodyContains "60.1"
  where
    bodyContains = WT.assertBodyContains . LBS.fromString
