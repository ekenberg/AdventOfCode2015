import Data.Char

header :: String
header = unlines [
  "import Data.Word",
  "import Data.Bits",
  "main = print var_a",
  "sigand, sigor :: Word16 -> Word16 -> Word16",
  "sigand    = (.&.)",
  "sigor     = (.|.)",
  "siglshift, sigrshift :: Word16 -> Int -> Word16",
  "siglshift = shiftL",
  "sigrshift = shiftR",
  "signot :: Word16 -> Word16",
  "signot    = complement"
  ]

parseCmd :: String -> String
parseCmd = handle . words
  where
    handle [y, "AND", z, _, x] = c x ++ " = sigand " ++ c y ++ " " ++ c z
    handle [y, "OR", z, _, x]  = c x ++ " = sigor "  ++ c y ++ " " ++ c z
    handle [y, "LSHIFT", z, _, x]  = c x ++ " = siglshift "  ++ c y ++ " " ++ c z
    handle [y, "RSHIFT", z, _, x]  = c x ++ " = sigrshift "  ++ c y ++ " " ++ c z
    handle ["NOT",y,_,x] = c x ++ " = signot " ++ c y
    handle [y, "->", x]  = c x ++ " = " ++ c y
    handle x             = error ("illegal command: " ++ unwords x)
    c t | isDigit (head t) = t
        | otherwise  = "var_" ++ t

main :: IO ()
main = do
  cmds <- unlines . map parseCmd . lines <$> readFile "input.txt"
  writeFile "runthis.hs" (header ++ cmds)
  putStrLn "Now compile runthis.hs and run the result."
  putStrLn "For the second part, edit runthis.hs manually for the updated input to var_b"
