import Data.List (sort)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [Int]
parse = map read . lines

solve :: [Int] -> Int
solve l = let l1 = sort l
              l2 = reverse l1
          in solve' l1 l2

solve' :: [Int] -> [Int] -> Int
solve' (x:xs) (y:ys) | x + y > 2020 = solve' (x:xs) ys
                     | x + y < 2020 = solve' xs (y:ys)
                     | otherwise    = x * y
