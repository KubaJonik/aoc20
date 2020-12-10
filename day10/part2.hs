
main = readFile "input.txt" >>= print . solve . parse

solve :: [Int] -> Integer
solve l = count 1 + count 2 + count 3
  where mv = maximum l
        count  = (map count' [0..] !!)
        count' n | n == mv = 1
                 | n `elem` l = count (n+1) + count (n+2) + count (n+3)
                 | otherwise = 0

parse :: String -> [Int]
parse = map read . lines
