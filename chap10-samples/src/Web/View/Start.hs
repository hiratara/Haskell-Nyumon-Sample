{-# LANGUAGE OverloadedStrings #-}
-- src/Web/View/Start.hs

module Web.View.Start (startView, loadStartTemplate) where

import qualified Data.Text     as TXT
import           Text.Mustache
    ( Template
    , automaticCompile
    , object
    , substitute
    , (~>)
    )
import           Web.Core
    ( WRAction
    , WRConfig (wrcTplRoots)
    , WRState (wrstStartTemplate)
    )
import           Web.Spock     (getState, html)

-- トップ画面のテンプレートを読み込む。
loadStartTemplate :: WRConfig -> IO Template
loadStartTemplate cfg = do
  compiled <- automaticCompile (wrcTplRoots cfg) "start.mustache"
  case compiled of
    Left err -> error (show err)
    Right template -> return template

-- 以前出てきたstartView関数は、トップ画面の表示を行う。
startView :: Maybe TXT.Text -> WRAction a
startView mMes = do
    tpl <- wrstStartTemplate <$> getState
    html $ substitute tpl $ object (toPairs mMes)  -- ブラウザ側にテンプレートから生成されたHTMLを返す。
  where
    toPairs (Just mes) = ["message" ~> mes]
    toPairs Nothing = []
