import Data.List

type Distance = Int
data Path = Path { from :: String,
                   to   :: String,
                   dist :: Distance }
  deriving (Eq, Show)

data City = City { name      :: String,
                   totaldist :: Distance,
                   notseen   :: [String], -- cities to visit
                   openpaths :: [Path],
                   nextstop  :: [City] -- where did we go from here
                 }
          | Done { finaldist :: Distance }
          | Fail
  deriving Show

explore :: City -> City
explore c | null (notseen c)   = Done (totaldist c)
          | null (openpaths c) = Fail -- no paths to use
          | null wherenow      = Fail -- nowhere to go
          | otherwise = c { nextstop = map (explore . nextCity) wherenow }
  where
    wherenow = filter (flip elem (notseen c) . to) $ filter ((== name c) . from) (openpaths c)
    nextCity p = City {name      = to p,
                       totaldist = totaldist c + dist p,
                       notseen   = delete (to p) (notseen c),
                       openpaths = filter (\pp -> from pp /= name c && to pp /= name c) (openpaths c),
                       nextstop  = [] }

uniqCities :: [Path] -> [String]
uniqCities = nub . map from

startCity :: [Path] -> String -> City
startCity p n = explore $ City { name      = n,
                                 totaldist = 0,
                                 notseen   = delete n (uniqCities p),
                                 openpaths = p,
                                 nextstop  = [] }

findShortest :: City -> Int
findShortest Fail     = -1
findShortest (Done n) = n
findShortest c        = minimum $ map findShortest (nextstop c)

findLongest :: City -> Int
findLongest Fail     = -1
findLongest (Done n) = n
findLongest c        = maximum $ map findLongest (nextstop c)

readPath :: String -> [Path]
readPath s = [Path f t (read d), Path t f (read d)]
  where [f,_,t,_,d] = words s

main :: IO ()
main = do
  pts <- concatMap readPath . lines <$> readFile "input.txt"
  let cs = map (startCity pts . from) pts
  let shortest = minimum $ map findShortest cs
  let longest = maximum $ map findLongest cs
  print shortest
  print longest
