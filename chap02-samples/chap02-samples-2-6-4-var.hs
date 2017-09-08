main = do
  let (x, y) = 22 `divMod` 5
  print x
  print y

  print $ let (x, y) = undefined in 1

  -- エラーが起こる
  -- print $ let f (x, y) = 1 in f undefined

  print $ let f ~(x, y) = 1 in f undefined
