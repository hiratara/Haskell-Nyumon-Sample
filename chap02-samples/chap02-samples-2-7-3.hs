import Data.Maybe (fromJust, isNothing)

percentage k n | n == 0    = Nothing              :: Maybe Double
               | otherwise = Just (100.0 * k / n)

main = do
  print $ case percentage 20 50 of Nothing -> "UNKNOWN"
                                   Just x  -> show x

  let p = percentage 20 50
  print $ if isNothing p then "UNKNOWN" else show (fromJust p)

  print $ maybe "UNKNOWN" show (percentage 20 50)
