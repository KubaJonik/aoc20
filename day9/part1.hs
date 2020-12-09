import Data.List (sort, scanl)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [Int] -> Int
solve ns = fst . head . filter (uncurry noPair) $ zs
  where (p,r) = splitAt 25 ns
        ls    = scanl (\l x -> tail l ++ [x]) p r
        zs    = zip r ls

noPair :: Int -> [Int] -> Bool
noPair n l = noPair' sl (reverse sl)
  where sl = sort l
        noPair' [] _ = True
        noPair' _ [] = True
        noPair' (x:xs) (y:ys) | x+y > n   = noPair' (x:xs) ys
                              | x+y < n   = noPair' xs (y:ys)
                              | otherwise = False

parse :: String -> [Int]
parse = map read . lines
