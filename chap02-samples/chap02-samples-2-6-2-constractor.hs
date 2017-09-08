main = do
  let x = (1, "one")
  print $ case x of (x1, x2) -> x2
  let y = (1, 10)
  print $ case y of (y1, y2) -> y1 + y2
