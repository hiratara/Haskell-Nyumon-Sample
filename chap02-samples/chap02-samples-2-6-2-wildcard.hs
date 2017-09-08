myDiv x = case x of (0, 0) -> 1         :: Int
                    (_, 0) -> maxBound
                    (n, m) -> n`div`m

main = do
  print $ myDiv (100, 10)
  print $ myDiv (100, 0)
  print $ myDiv (0, 0)
