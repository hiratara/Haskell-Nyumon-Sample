import Control.Monad
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  -- 初期化
  animals <- VM.new 5 -- 5 要素の配列を作成
  VM.write animals 0 "Dog"
  VM.write animals 1 "Pig"
  VM.write animals 2 "Cat"
  VM.write animals 3 "Fox"
  VM.write animals 4 "Mouse"

  -- インデックス 1 と 3 の要素を入れ替える
  tmp <- VM.read animals 1
  VM.write animals 1 =<< VM.read animals 3
  VM.write animals 3 tmp

  -- 表示
  forM_ [0 .. (VM.length animals - 1)] $ \i -> do
    putStrLn =<< VM.read animals i
