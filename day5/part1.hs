
main :: IO ()
main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve = maximum . map seatId . lines

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

