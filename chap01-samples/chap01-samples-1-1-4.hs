main = do
  let path = "README.md"
  body <- readFile path
  putStrLn body
