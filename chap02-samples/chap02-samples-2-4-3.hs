add m n = m + n :: Int

main = print $ add 1 $ let x = 10
                           y = 20
                       in x + y
