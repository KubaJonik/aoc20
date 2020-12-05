import Data.List (sort)

main :: IO ()
main = readFile "input.txt" >>= print . findSeat . seats

findSeat :: [Int] -> Int
findSeat (x:y:rs) | x+1 == y-1 = x+1
                  | otherwise = findSeat (y:rs)

seats :: String -> [Int]
seats = sort . map seatId . lines

seatId :: String -> Int
seatId s = let (row, col) = splitAt 7 s
           in 8 * (rowId row) + colId col

rowId :: String -> Int
rowId = binVal 'F' 'B'

colId :: String -> Int
colId = binVal 'L' 'R'

binVal :: Char -> Char -> String -> Int
binVal z o s = binVal' s 0
  where binVal' "" acc = acc
        binVal' (x:xs) acc | x == z = binVal' xs (2*acc)
                           | x == o = binVal' xs (2*acc + 1)

