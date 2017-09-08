import Data.Either

percentage k n | n == 0    = Left "Illegal division by zero"
               | otherwise = Right (100.0 * k / n) :: Either String Double

main = do
  print $ case percentage 20 50 of Left err -> err
                                   Right x  -> show x

  print $ either id show (percentage 20 50)
