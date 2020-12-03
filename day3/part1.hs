
main :: IO ()
main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve s = count idxs . lines $ s
  where k = length . head . lines $ s
        idxs = map (`mod` k) [0,3..]

count :: [Int] -> [String] -> Int
count _ [] = 0
count (p:ps) (s:ss) = case (s !! p) of
                            '.' -> count ps ss
                            '#' -> 1 + count ps ss
