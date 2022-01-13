import Data.List

isNicer :: String -> Bool
isNicer xs = and $ [doublePairs, separatedPair] <*> [xs]

doublePairs :: String -> Bool
doublePairs (x:y:xs) | [x,y] `isInfixOf` xs = True
                     | otherwise = doublePairs (y:xs)
doublePairs _ = False

separatedPair :: String -> Bool
separatedPair (x:y:z:xs) | x == z = True
                         | otherwise = separatedPair (y:z:xs)
separatedPair _ = False
--
isNice :: String -> Bool
isNice xs = and $ [enoughVowels, doubleLetter, noForbidden] <*> [xs]

enoughVowels :: String -> Bool
enoughVowels = check 0
  where
    check :: Int -> String -> Bool
    check n [] = n >= 3
    check n (x:xs) | n >= 3 = True
                   | otherwise = check (if [x] `isInfixOf` "aeiou" then n+1 else n) xs

doubleLetter :: String -> Bool
doubleLetter = any ((> 1) . length) . group

noForbidden :: String -> Bool
noForbidden xs = none $ map (`isInfixOf` xs) ["ab", "cd", "pq", "xy"]

none :: Foldable t => t Bool -> Bool
none = not . or

main :: IO ()
main = do
  wrds <- lines <$> readFile "input.txt"
  print $ length $ filter isNice wrds
  print $ length $ filter isNicer wrds
