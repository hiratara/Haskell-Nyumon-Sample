module Main(main) where

import Pipes
import qualified Pipes.Prelude as P

sample1 :: IO ()
sample1 = runEffect $ sampleProducer >-> P.map ("input : "++) >-> P.stdoutLn

sampleProducer :: Producer String IO ()
sampleProducer = do
  yield "Hoge"
  yield "Piyo"
  yield "Fuga"

sample2 :: IO ()
sample2 = runEffect $ range 1 20 >-> sampleConsumer

-- nからm までの範囲を表す簡単なProducerを
-- Control.Monadで提供されているmapM_関数を利用して作成する
range :: Int -> Int -> Producer Int IO ()
range n m = mapM_ yield [n..m]

sampleConsumer :: Consumer Int IO ()
sampleConsumer = do
  -- await関数でストリームの上流からデータを取得する
  x <- await
  -- fizzBuzzして出力
  lift . putStrLn $ fizzBuzz x
  -- ストリームデータの最後まで自分自身を呼び出す
  sampleConsumer

--簡単なFizzBuzzの実装
fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

main :: IO ()
main = do
  sample1
  sample2
