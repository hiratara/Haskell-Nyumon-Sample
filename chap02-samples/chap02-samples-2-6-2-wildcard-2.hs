f x y = case (x, y) of (True, False) -> False
                       _             -> True

-- 警告が出る例
f' x y = case (x, y) of _             -> True
                        (True, False) -> False

main = do
  print $ f True True
  print $ f False True
  print $ f True False
  print $ f' True True
  print $ f' False True
  print $ f' True False
