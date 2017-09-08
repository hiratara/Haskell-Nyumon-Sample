import Control.Concurrent

main :: IO ()
main = do
  -- スレッド間で共有するMVar生成
  m <- newEmptyMVar

  -- スレッド生成
  forkIO $ do
    tid <- myThreadId -- 現在のスレッドIDを取得
    putStrLn $ show tid ++ ": doing ... heavy ... task ..."
    threadDelay 2000000 -- マイクロ秒単位でスレッド実行を停止
    putMVar m () -- 処理が終わったことを通知

  takeMVar m -- 生成したスレッドから通知が来るまで待ち続ける
  putStrLn "Done"
