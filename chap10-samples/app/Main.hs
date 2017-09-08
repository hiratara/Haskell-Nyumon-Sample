-- app/Main.hs
module Main (main) where

import Data.Monoid ((<>))
import Options.Applicative
    ( Parser
    , auto
    , execParser
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , value
    )
import Paths_weight_recorder (getDataDir)
import System.FilePath       ((</>))
import Web.WeightRecorder    (WRConfig (WRConfig), runWeightRecorder)

buildCfgParser :: IO (Parser WRConfig)
buildCfgParser = do
  datadir <- getDataDir
  let db =
        option
          auto
          (long "db" <> short 'd' <> metavar "DB" <> help "SQLite DB" <>
           value "weight.db")
      tplroot =
        option
          auto
          (long "tplroot" <> short 't' <> metavar "ROOT" <>
           help "the root path of template directory" <>
           value (datadir </> "templates"))
      port =
        option
          auto
          (long "port" <> short 'p' <> metavar "PORT" <>
           help "listen PORT" <>
           value 8080)
  return $ WRConfig <$> db <*> ((: []) <$> tplroot) <*> port

main :: IO ()
main = do
  parser <- buildCfgParser
  let opts =
        info
          (helper <*> parser)
          (progDesc "Run the Weight Recorder server" <>
           header
             "weight-recorder - A web application to record your weights")
  cfg <- execParser opts
  -- アプリケーションをコマンドラインオプションを渡して起動。
  runWeightRecorder cfg
