main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ foldl (\n c->if c == '(' then n+1 else n-1) 0 input
