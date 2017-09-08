module Main(main) where

import Data.Time (UTCTime(..), addDays, addUTCTime, getCurrentTime)

add2DayFromNow :: IO ()
add2DayFromNow = do
  -- パターンマッチでDay型の値を取得する
  utcTime@(UTCTime day diffTime) <- getCurrentTime
  print utcTime
  -- addDaysで2日分足してUTCTimeに戻す
  let newTime = UTCTime (addDays 2 day) diffTime
  print newTime

add1HourFromNow :: IO ()
add1HourFromNow = do
  utcTime <- getCurrentTime
  print utcTime
  -- (60*60)秒 = 1時間
  let newTime = addUTCTime (60*60) utcTime
  print newTime

main :: IO ()
main = do
  add2DayFromNow
  add1HourFromNow
