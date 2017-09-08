f x = case x of n | n`mod`2 == 0 -> putStrLn "even"
                  | otherwise    -> putStrLn "odd"

g x = case x of (x1, True) | x1`mod`2 == 0 -> x1`div`2
                (x1, _)    | x1`mod`2 == 0 -> x1
                           | otherwise     -> x1 - 1

h x = case x of (x1, True) | (q, 0) <- x1 `divMod` 2 -> q
                (x1, _   )                           -> x1

main = do
  putStrLn "Calling f"
  f 10
  f 11

  putStrLn "Calling g"
  print $ g (10, True)
  print $ g (11, True)
  print $ g (10, False)

  putStrLn "Calling h"
  print $ h (10, True)
  print $ h (10, False)
  print $ h (11, True)
