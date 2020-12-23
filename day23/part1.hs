
input = "219748365" :: String
puzzle = map (\c -> read [c]) input :: [Int]
minVal = minimum puzzle
maxVal = maximum puzzle

main :: IO ()
main = putStrLn . solve $ puzzle

solve :: [Int] -> String
solve = concatMap show . tail . (moveToFront 1) . head . drop 100 . iterate move

move :: [Int] -> [Int]
move (d:ds) = tail ds' ++ [head ds']
  where (ts, rs) = (take 3 ds, drop 3 ds)
        nd       = nextDest d ts
        ds'      = insert nd ts (d:rs)

insert :: Int -> [Int] -> [Int] -> [Int]
insert d tcups (c:cs) | c == d = c : (tcups ++ cs)
                      | otherwise = c : insert d tcups cs

nextDest :: Int -> [Int] -> Int
nextDest d tcups = nextDest' (decDest d)
  where nextDest' d' | d' `notElem` tcups = d'
                     | otherwise = nextDest' (decDest d')

decDest :: Int -> Int
decDest n | n == minVal = maxVal
          | otherwise = n - 1

moveToFront :: Int -> [Int] -> [Int]
moveToFront d (x:xs) | x == d    = (x:xs)
                     | otherwise = moveToFront d (xs ++ [x])
