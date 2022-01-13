main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ foldl (\n c->if c == '(' then n+1 else n-1) 0 input
  print $ fst . head $ filter ((==(-1)) . snd) $
    scanl (\(p,n) c -> if c == '(' then (p+1, n+1) else (p+1, n-1)) (0,0) input
