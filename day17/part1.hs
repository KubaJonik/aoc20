import qualified Data.Set as S
import Data.Set (Set, member, elems)
import Data.List (nub)

type Co = (Int, Int, Int)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: Set Co -> Int
solve = S.size . head . drop 6 . iterate step

step :: Set Co -> Set Co
step s = S.fromList (activeOld ++ activeNew)
  where activeOld = filter (stayActive s) (findActive s)
        activeNew = filter (turnActive s) (findInactive s)

stayActive :: Set Co -> Co -> Bool
stayActive s c = let count = countActive s c
                   in count == 2 || count == 3

turnActive :: Set Co -> Co -> Bool
turnActive s c = (countActive s c) == 3

findInactive :: Set Co -> [Co]
findInactive s = nub [n | p <- elems s, n <- nbs p, not (member n s)]

findActive :: Set Co -> [Co]
findActive = elems

countActive :: Set Co -> Co -> Int
countActive s = length . filter (flip member s) . nbs

nbs :: Co -> [Co]
nbs c = map (add c) dnbs
  where add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
        dnbs = [(x,y,z) | x <- [-1..1],
                          y <- [-1..1],
                          z <- [-1..1],
                          (x,y,z) /= (0,0,0)]

parse :: String -> Set Co
parse s = S.fromList . concat $ [parseLine y l | (y, l) <- [0..] `zip` (lines s)]
  where parseLine y l = [(x, y, 0) | (x, c) <- [0..] `zip` l, c == '#']
