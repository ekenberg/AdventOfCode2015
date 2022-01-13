import Data.List

paperNeeded :: [Int] -> Int -> Int
paperNeeded [x,y,z] prev = prev + (2 * (x*y + x*z + y*z) + x*y)

ribbonNeeded :: [Int] -> Int -> Int
ribbonNeeded [x,y,z] prev = prev + x*y*z + 2 * (x+y)

parseItem :: String -> [Int]
parseItem xs = sort [l,w,d]
  where
    (l, rst)  = head $ reads xs
    (w, rst2) = head $ reads $ tail rst
    (d, _)    = head $ reads $ tail rst2

main :: IO ()
main = do
  lns <- map parseItem . lines <$> readFile "input.txt"
  print $ foldr paperNeeded 0 lns
  print $ foldr ribbonNeeded 0 lns
