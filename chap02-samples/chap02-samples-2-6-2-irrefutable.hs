heavyPred n = length (replicate (n * 300000) 'x') > 1000000

f n = if heavyPred n then Just n else Nothing

main = do
  putStrLn "Start pattern 1"
  print $ case f 10000 of Just n -> n
  putStrLn "End pattern 1"

  putStrLn "Start pattern 2"
  print $ case f 10000 of Just n -> 0
  putStrLn "End pattern 2"

  putStrLn "Start pattern 3"
  print $ case f 10000 of ~(Just n) -> 0
  putStrLn "End pattern 3"
