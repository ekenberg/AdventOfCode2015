

diffString :: String -> Int
diffString raw = length raw - inside 0 raw
  where
    inside n [] = n
    inside n ('"':xs) = inside n xs
    inside n ('\\':'x':xs) = inside (n+1) (drop 2 xs) -- hex
    inside n ('\\':_:xs)   = inside (n+1) xs
    inside n (x:xs)        = inside (n+1) xs

diffString2 :: String -> Int
diffString2 raw = encode 2 raw - length raw
  where
    encode n [] = n
    encode n ('"':xs) = encode (n+2) xs
    encode n ('\\':xs)   = encode (n+2) xs
    encode n (x:xs)      = encode (n+1) xs



main :: IO ()
main = do
  diff <- sum . map diffString . lines <$> readFile "input.txt"
  print diff
  diff2 <- sum . map diffString2 . lines <$> readFile "input.txt"
  print diff2
