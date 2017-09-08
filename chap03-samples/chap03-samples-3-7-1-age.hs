type Age = Integer -- typeに続き左辺に新しい型名、右辺にもともとの型名を書く

legalDrink :: Age -> Bool
legalDrink age | age >= 20 = True
               | otherwise = False

main :: IO ()
main = do
  let age = 24 :: Age
      n   = -1 :: Integer
  print $ legalDrink age
  print $ legalDrink n
