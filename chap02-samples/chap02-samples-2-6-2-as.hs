myDiv x = case x of (0, 0) -> 1         :: Int
                    (_, 0) -> maxBound
                    (n, m) -> n`div`m

divOrMyDiv x = case x of (x'@(_, 0), True) -> myDiv x'
                         (   (n, m), _   ) -> n`div`m

main = do
  print $ divOrMyDiv ((5, 2), False)
  -- 実行時エラーとなる
  -- print $ divOrMyDiv ((0, 0), False)
  print $ divOrMyDiv ((0, 0), True)
