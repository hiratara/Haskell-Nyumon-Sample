f (x1, True) | (q, 0) <- x1 `divMod` 2 = q
f (x1, _   )                           = x1

g n | n`mod`2 == 0 = putStrLn "even"
    | otherwise    = putStrLn "odd"

h = \(x, y) z -> x + y + z

main = do
  print $ f (11, True)
  g 19
  print $ h (1, 2) 3
