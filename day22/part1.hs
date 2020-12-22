
type Deck = [Int]

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: (Deck, Deck) -> Int
solve = sum . zipWith (*) [1..] . reverse . newRound

newRound :: ([Int], [Int]) -> [Int]
newRound (d, []) = d
newRound ([], d) = d
newRound (c1:cs1, c2:cs2) | c1 > c2 = newRound (cs1 ++ [c1,c2], cs2)
                          | c2 > c1 = newRound (cs1, cs2 ++ [c2,c1])

parse :: String -> ([Int],[Int])
parse s = (map read . drop 1 $ p1, map read . drop 2 $ p2)
  where (p1, p2) = break null . lines $ s

