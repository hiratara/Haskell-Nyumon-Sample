{-# LANGUAGE GADTs #-}

import Data.List
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Operational -- --pacakge operationalを指定する

-- データ型定義
type Price = Int --単価
type Amount = Int --個数
type Product = String --商品名
type Report = [(Product, Amount)] -- 売り上げレポート
type ProductList = [(Product, Price)] -- 商品一覧

-- 操作のためのAPI定義
data SalesBase a where
  GetProducts :: SalesBase ProductList
  GetReport :: SalesBase Report
  Sell :: (Product, Amount) -> SalesBase ()

-- 簡略化のための型シノニム
type SalesT m a = ProgramT SalesBase m a
type Sales a = Program SalesBase a

getProducts :: SalesT m ProductList
getProducts = singleton GetProducts

getReport :: SalesT m Report
getReport = singleton GetReport

sell :: (Product, Amount) -> SalesT m ()
sell p = singleton $ Sell p

-- モナドなのでdo式が使える
sellFruits :: Sales ()
sellFruits = do
  sell ("Apple", 5)
  sell ("Grape", 8)
  sell ("Pineapple", 2)

runSalesT :: Monad m => ProductList -> Report -> SalesT m a -> m a
runSalesT p r = viewT >=> eval -- '>=>'演算子を使うと左辺のモナドの計算結果を右辺のモナドに渡せます
  where
    -- eval関数は、SalesBase型の値ごとの、実際の処理を実行します
    -- runSalesT関数に後続の処理を渡し、再帰的に呼び出すことでdo式で書かれた複数行のプログラムを処理できます
    eval :: Monad m => ProgramViewT SalesBase m a -> m a
    eval (Return x) = return x
    -- getProducts関数の処理、引数のProductListをkに渡して次のアクションで利用できるようにします
    eval (GetProducts :>>= k) = runSalesT p r (k p)
    -- getReport関数の処理、getProducts関数の場合と同様です
    eval (GetReport :>>= k) = runSalesT p r (k r)
    eval (Sell s :>>= k) = runSalesT p (s:r) (k ())

-- Salesモナドをモナド変換子として使わない場合、runIdentity で m をIdentity に限定します
runSales :: ProductList -> Report -> Sales a -> a
runSales p r = runIdentity . runSalesT p r

-- find関数を使ってProductListから金額を取り出します。find関数の結果はMaybe型なのでsnd関数をfmapしています
findAmount :: Product -> ProductList -> Maybe Price
findAmount p = fmap snd . find ((p==).fst)

-- ここまでで作成した関数を組みあわせて、商品毎の売り上げを集計する関数を実装します
summary :: Sales [(Product, Amount, Maybe Price)]
summary = let
    sumRecord :: ProductList -> (Product, Amount) -> (Product, Amount, Maybe Price)
    sumRecord px (p, n) = (p, n, fmap (*n) $ findAmount p px)
  in do
    list <- getProducts
    report <- getReport
    return $ map (sumRecord list) report

productList :: ProductList
productList =
  [ ("Apple", 98)
  , ("Grape", 398)
  , ("Pineapple", 498)
  ]

main :: IO ()
main = do
  -- 先ほど作成したsaleFruits関数で登録したデータを、summary関数で集計します
  print $ runSales productList [] (sellFruits >> summary)
