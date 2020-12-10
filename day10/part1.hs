import Data.List (sort, group)

main = readFile "input.txt" >>= print . solve . map read . lines
solve l = product . map length . group . sort . filter (/=2) . zipWith (-) (sort l ++ [3 + maximum l]) $ 0 : sort l
