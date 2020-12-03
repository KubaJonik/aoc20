
main :: IO ()
main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve s = pr [(1,1), (3,1), (5,1), (7,1), (1,2)]
    where ls = lines s
          pr = product . map (\(r,d) -> count r d ls)

count :: Int -> Int -> [String] -> Int
count r d ls  = count' d ps ls
    where k  = length . head $ ls
          ps = map (`mod` k) [0,r..]

count' :: Int -> [Int] -> [String] -> Int
count' _ _ [] = 0
count' d (p:ps) (s:ss) = case (s !! p) of
                              '.' -> count' d ps s'
                              '#' -> 1 + count' d ps s'
                         where s' = drop (d-1) ss
