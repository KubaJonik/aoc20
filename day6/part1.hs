import Data.List (sort,group)

main = readFile "input.txt" >>= print . solve . parse . lines

solve :: [String] -> Int
solve = sum . map count

count :: String -> Int
count = length . map head . group . sort

parse :: [String] -> [String]
parse s = case span (/="") s of
            (h, []) -> [concat h]
            (h, s') -> concat h : parse (tail s')
