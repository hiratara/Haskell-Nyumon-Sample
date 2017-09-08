x = 4 :: Int

main = print $ case x of 1  -> "1st"
                         2  -> "2nd"
                         3  -> "3rd"
                         x' -> show x' ++ "th"
