import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, keys, elems, lookup, (!))
import Data.Maybe (catMaybes)

type Grid = Map (Int,Int) Char

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: Grid -> Int
solve = length . filter (=='#') . elems . fixPoint

fixPoint :: Grid -> Grid
fixPoint grid | grid == grid' = grid
              | otherwise     = fixPoint grid'
    where grid' = step grid

step :: Grid -> Grid
step grid = M.fromList [(k,v) | k <- keys grid,
                                let v = newState k grid]

newState :: (Int, Int) -> Grid -> Char
newState pos grid = case grid ! pos of
                          'L' -> newE (adj pos grid)
                          '#' -> newO (adj pos grid)
                          '.' -> '.'

newE :: [Char] -> Char
newE cs | any (=='#') cs = 'L'
        | otherwise      = '#'

newO :: [Char] -> Char
newO cs | occ >= 5  = 'L'
        | otherwise = '#'
    where occ = length . filter (=='#') $ cs

adj :: (Int, Int) -> Grid -> [Char]
adj (row,col) grid = catMaybes $ [adj' r c | r <- [-1..1], c <- [-1..1], r /= 0 || c /= 0]
  where adj' dr dc = head . dropWhile (== Just '.') . map (flip lookup grid) $ pos dr dc
        pos  dr dc = [row+dr, row+2*dr..] `zip` [col+dc, col+2*dc..]

parse :: String -> Grid
parse = M.fromList . concatMap parseRow . zip [0..] . lines
    where parseRow (r, row) = [((r,c), x) | (c, x) <- zip [0..] row ]

