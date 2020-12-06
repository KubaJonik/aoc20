import Data.List (sort,group,groupBy)

main = readFile "input.txt" >>= print . solve . parse

solve :: [String] -> Int
solve = sum . map count

count :: String -> Int
count = length . map head . group . sort

parse :: String -> [String]
parse = map concat . groupBy nonWhite . lines
  where nonWhite x y = x/="" && y/=""

