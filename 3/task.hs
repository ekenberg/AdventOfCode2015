import Data.List

type Coord = (Int, Int)

main :: IO ()
main = do
  instructions <- head . lines <$> readFile "input.txt"
  print $ count $ follow instructions
  print $ count $ follow (robot instructions) ++ follow (santa instructions)

count :: [Coord] -> Int
count = length . group . sort

follow :: String -> [Coord]
follow = scanl navigate (0,0)

santa :: String -> String
santa = everyother 0

robot :: String -> String
robot = everyother 1

everyother :: Int -> String -> String
everyother _ [] = []
everyother w (x:xs) | w == 0 = x : everyother 1 xs
                    | otherwise = everyother 0 xs

navigate :: Coord -> Char -> Coord
navigate (fx,fy) c | c == '^' = (fx, fy+1)
                   | c == 'v' = (fx, fy-1)
                   | c == '>' = (fx+1, fy)
                   | c == '<' = (fx-1, fy)
navigate _ _ = error "navigational error"
