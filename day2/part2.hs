data Entry = Entry { lower    :: Int,
                     upper    :: Int,
                     letter   :: Char,
                     password :: String }

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [Entry] -> Int
solve = length . filter valid

valid :: Entry -> Bool
valid entry = let (p1, p2)   = (lower entry, upper entry)
                  (ltr, pwd) = (letter entry, password entry)
              in (pwd !! (p1-1) == ltr) /= (pwd !! (p2-1) == ltr)

parse :: String -> [Entry]
parse = map parseLine . lines

parseLine :: String -> Entry
parseLine s = Entry (read lwr) (read upr) ltr pwd
  where (lwr, rs1) = span (/= '-') s
        (upr, rs2) = span (/= ' ') (tail rs1)
        (ltr, rs3) = (head.tail $ rs2, tail rs2)
        pwd        = drop 3 rs3

