module Main(main) where

-- 型が少しややこしくなるので別名を付けておく
type Category = String -- 商品のカテゴリ
type Name = String -- 商品名
type Price = Integer -- 金額
type Menu = [(Category, [(Name, Price)])] -- メニュー
type Item = (Category, Name, Price) -- 商品

-- メニューデータの例
sampleMenu :: Menu
sampleMenu =
  [ ("Foods", -- 食事
    [ ("Hamburger", 120) -- ハンバーガー
    , ("FrenchFries", 100) --ポテト
    ] )
  , ("Drinks",  -- 飲みもの
    [ ("Cola", 80) -- コーラ
    , ("Tea", 100) -- お茶
    ] )
  ]

getItem :: Menu -> Category -> Name -> Maybe Item
getItem menu category name
  -- パターンマッチするたびにネストが増えてしまう
  = case lookup category menu of
    Just subMenu -> case lookup name subMenu of
      Just price -> Just (category, name, price)
      -- マッチしなかった場合はNothing
      Nothing -> Nothing
    -- この計算ではマッチしなかった時の処理はNothingと決まっているので何度も書きたくない
    Nothing -> Nothing

getItemWithMonad :: Menu -> Category -> Name -> Maybe Item
getItemWithMonad menu category name = do
  -- IOでgetLineしたときのように、lookup関数が使える
  subMenu <- lookup category menu
  price <- lookup name subMenu
  -- どこかのアクションがNothingを返せば、計算全体がNothingとなる
  -- 最終的に欲しい結果を組み立ててreturnで返す
  return (category, name, price)

main :: IO ()
main = do
  print $ getItem sampleMenu "Foods" "Hamburger"
  print $ getItemWithMonad sampleMenu "Foods" "Hamburger"
