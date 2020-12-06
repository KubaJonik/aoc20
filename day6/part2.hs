import Data.List (sort,group,intersect)

main = readFile "input.txt" >>= print . solve . parse . lines

solve :: [[String]] -> Int
solve = sum . map count

count :: [String] -> Int
count (x:xs) = length (foldl intersect x xs)

parse :: [String] -> [[String]]
parse s = case span (/="") s of
            (h, []) -> [h]
            (h, s') -> h : parse (tail s')
