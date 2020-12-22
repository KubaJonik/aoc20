
data Player = ONE | TWO

type Deck = [Int]

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: (Deck, Deck) -> Int
solve = sum . zipWith (*) [1..] . reverse . snd . newGame

newRound :: [(Deck, Deck)] -> (Deck, Deck) -> (Player, Deck)
newRound _ (cs, []) = (ONE, cs)
newRound _ ([], cs) = (TWO, cs)
newRound his ds@(d1@(c1:cs1), d2@(c2:cs2))
    | ds `elem` his = (ONE, d1)
    | isNewGame d1 d2 = case newGame (take c1 cs1, take c2 cs2) of
                           (ONE,_) -> newRound (ds:his) (cs1 ++ [c1,c2], cs2)
                           (TWO,_) -> newRound (ds:his) (cs1, cs2 ++ [c2,c1])
    | c1 > c2 = newRound (ds:his) (cs1 ++ [c1,c2], cs2)
    | c1 < c2 = newRound (ds:his) (cs1, cs2 ++ [c2,c1])

newGame :: (Deck, Deck) -> (Player, Deck)
newGame = newRound []

isNewGame :: Deck -> Deck -> Bool
isNewGame (c1:cs1) (c2:cs2) = c1 <= length cs1 && c2 <= length cs2

parse :: String -> (Deck, Deck)
parse s = (map read . drop 1 $ p1, map read . drop 2 $ p2)
  where (p1, p2) = break null . lines $ s

