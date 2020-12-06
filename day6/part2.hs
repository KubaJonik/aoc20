import Data.List (groupBy, intersect)

main = readFile "input.txt" >>= print . solve . parse

solve :: [[String]] -> Int
solve = sum . map count

count :: [String] -> Int
count []     = 0
count (x:xs) = length (foldl intersect x xs)

parse :: String -> [[String]]
parse = groupBy nonWhite . lines
  where nonWhite x y = x/="" && y/=""

