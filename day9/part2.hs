import Data.List (sort, scanl)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [Int] -> Int
solve ls = (minimum cr) + (maximum cr)
  where n  = findN ls
        cr = findS 0 [] ls
        findS s l1 l2 | s == n = l1
                      | s < n = findS (s + head l2) (l1 ++ [head l2]) (tail l2)
                      | s > n = findS (s - head l1) (tail l1) l2

findN :: [Int] -> Int
findN ns = fst . head . filter (uncurry noPair) $ zs
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
