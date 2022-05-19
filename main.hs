import Scanner (tokens)

main :: IO ()
main = do
  input <- getContents
  mapM_ (putStrLn . show) (tokens input)