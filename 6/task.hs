import Data.List
import Data.Char

data Action = TurnOn Pos Pos | TurnOff Pos Pos | Toggle Pos Pos deriving Show
data Light  = On | Off | Glow Int deriving Eq
type Pos = (Int, Int)
type Grid = [[Light]]

instance Show Light where
  show On = "x"
  show Off = " "
  show (Glow n) = show n

glowGrid :: Grid
glowGrid = replicate 1000 $ replicate 1000 (Glow 0)

testglowGrid :: Grid
testglowGrid = replicate 10 $ replicate 10 (Glow 0)

offGrid :: Grid
offGrid = replicate 1000 $ replicate 1000 Off

testoffGrid :: Grid
testoffGrid = replicate 10 $ replicate 10 Off

toggle :: Light -> Light
toggle On  = Off
toggle Off = On
toggle (Glow n) = Glow (n+2)

turnon :: Light -> Light
turnon (Glow n) = Glow (n+1)
turnon _ = On

turnoff :: Light -> Light
turnoff (Glow n) = Glow (max (n-1) 0)
turnoff _ = Off

apply :: Action -> Grid -> Grid
apply a g = case a of
              (TurnOn p1 p2)  -> switch g turnon  p1 p2
              (TurnOff p1 p2) -> switch g turnoff p1 p2
              (Toggle p1 p2)  -> switch g toggle  p1 p2

switch :: Grid -> (Light -> Light) -> Pos -> Pos -> Grid
switch g f (p1x, p1y) (p2x, p2y) =
  take p1y g ++ map switchRow (take dy (drop p1y g)) ++ drop (p2y + 1) g
  where
    switchRow r = take p1x r ++ map f (take dx (drop p1x r)) ++ drop (p2x + 1) r
    dy = p2y - p1y + 1
    dx = p2x - p1x + 1

countOn :: Grid -> Int
countOn = length . filter (== On) . concat

totalBrightness :: Grid -> Int
totalBrightness = sum . map brightness . concat
  where
    brightness (Glow n) = n
    brightness _        = error "this is not the light you're looking for"

parseRow :: String -> Action
parseRow row | "turn on" `isPrefixOf` row  = TurnOn fp sp
             | "turn off" `isPrefixOf` row = TurnOff fp sp
             | "toggle" `isPrefixOf` row   = Toggle fp sp
             | otherwise = error "illegal command"
  where
    (fp, sp)    = ((read fpx, read fpy), (read spx, read spy))
    (fpx, rst)  = span isDigit $ dropWhile (not . isDigit) row
    (fpy, rst2) = span isDigit $ tail rst
    (spx, rst3) = span isDigit $ dropWhile (not . isDigit) rst2
    (spy, _)    = span isDigit $ tail rst3

main :: IO ()
main = do
  commands <- map parseRow . lines <$> readFile "input.txt"
  let test_commands = [TurnOn (1,1) (2,2), Toggle (0,1) (9,1), Toggle (1,2) (5,7)]

  -- let test_bool_grid = foldl (flip apply) testoffGrid test_commands
  -- mapM_ print test_bool_grid
  -- print $ countOn test_bool_grid
  let bool_grid = foldl (flip apply) offGrid commands
  print $ countOn bool_grid

  -- let test_glow_grid = foldl (flip apply) testglowGrid test_commands
  -- mapM_ print test_glow_grid
  -- print $ totalBrightness test_glow_grid
  let glow_grid = foldl (flip apply) glowGrid commands
  print $ totalBrightness glow_grid
