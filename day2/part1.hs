
data Entry = Entry { lower    :: Int,
                     upper    :: Int,
                     letter   :: Char,
                     password :: String }

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [Entry] -> Int
solve = length . filter valid

valid entry = let count = length $ filter (==letter entry) (password entry)
              in (lower entry) <= count && count <= (upper entry)

parse :: String -> [Entry]
parse = map parseLine . lines

parseLine :: String -> Entry
parseLine s = Entry (read lwr) (read upr) ltr pwd
  where (lwr, rs1) = span (/= '-') s
        (upr, rs2) = span (/= ' ') (tail rs1)
        (ltr, rs3) = (head.tail $ rs2, tail rs2)
        pwd        = drop 3 rs3


