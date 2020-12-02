import Data.List (sort)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [Int]
parse = map read . lines

solve :: [Int] -> Int
solve l = head [x * p | x <- l1, p <- solve' x l2 l3]
  where l1 = filter (< 2020) l
        l2 = sort l
        l3 = reverse l2

solve' :: Int -> [Int] -> [Int] -> [Int]
solve' _ [] _ = []
solve' _ _ [] = []
solve' k (x:xs) (y:ys) | x+y+k > 2020 = solve' k (x:xs) ys
                       | x+y+k < 2020 = solve' k xs (y:ys)
                       | otherwise    = [x * y]

