module Main (main) where

data TreeDict k v = TDEmpty
                  | TDNode k v (TreeDict k v) (TreeDict k v)
                  deriving (Show)

-- k（Ordの型制約）、v、TreeDict型を引数にTreeDict型を返す。
insert :: Ord k => k -> v -> TreeDict k v -> TreeDict k v
-- 第三引数のTreeDictの型でinsertの実装を分ける。
-- TDEmptyコンストラクタの場合。
insert k v TDEmpty = TDNode k v TDEmpty TDEmpty
-- TDNodeコンストラクタの場合。
insert k v (TDNode k' v' l r)
  | k < k'    = TDNode k' v' (insert k v l) r
  | k > k'    = TDNode k' v' l              (insert k v r)
  | otherwise = TDNode k' v  l              r

dict :: TreeDict String Integer
dict = insert "hiratara" 39
     . insert "shu1"      0
     . insert "masahiko" 63
     $ TDEmpty

lookup' :: Ord k => k -> TreeDict k v -> Maybe v
lookup' _ TDEmpty = Nothing
lookup' k (TDNode k' v' l r)
  | k < k'    = lookup' k l
  | k > k'    = lookup' k r
  | otherwise = Just v'

main :: IO ()
main = do
  print $ lookup' "hiratara" dict
  print $ lookup' "pinnyu"   dict
