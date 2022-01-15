import Data.List

twoPairs :: String -> Bool
twoPairs = (>1) . length . filter ((>1) . length) . group

badChars :: String
badChars = "iol"

okChars :: String -> Bool
okChars = null . intersect badChars

threeStraight :: String -> Bool
threeStraight (x:y:z:rst) | y == succ x && z == succ y = True
                          | otherwise = threeStraight (y:z:rst)
threeStraight _ = False

okPasswd :: String -> Bool
okPasswd = and . ([twoPairs, okChars, threeStraight] <*>) . (:[])

nextPasswd :: String -> String
nextPasswd x = head $ filter okPasswd $ iterate stringSucc (stringSucc x)

stringSucc :: String -> String
stringSucc [] = "a"
stringSucc xs | last xs == 'z' = skipBad (stringSucc (init xs) ++ "a")
              | otherwise      = skipBad (init xs ++ [succ (last xs)])
  where
    skipBad (y:ys) | y `elem` badChars = succ y : replicate (length ys) 'a'
                   | otherwise           = y : skipBad ys
    skipBad _ = []

oldPasswd :: String
oldPasswd = "hxbxwxba"

main :: IO ()
main = do
  let pwd = nextPasswd oldPasswd
  let pwd2 = nextPasswd $ nextPasswd oldPasswd
  print pwd
  print pwd2
