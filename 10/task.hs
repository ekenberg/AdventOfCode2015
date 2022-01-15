import Data.List

lookAndSay :: String -> String
lookAndSay = concatMap say . group
  where say ys = show (length ys) ++ [head ys]

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)

start :: String
start = "1113122113"

main :: IO ()
main = do
  print $ length $ applyN 40 lookAndSay start
  print $ length $ applyN 50 lookAndSay start
