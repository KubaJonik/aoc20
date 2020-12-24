import Prelude hiding (round)
import Data.List (nub)
import Data.Set (Set, member, elems)
import qualified Data.Set as S

data Dir = E | SE | SW | W | NW | NE deriving Enum
type Tile = (Int, Int)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve = length . head . drop 100 . iterate round . blacks

round :: Set Tile -> Set Tile
round blacks = S.fromList (blacksOld ++ blacksNew)
  where blacksOld = filter (stayBlack blacks) (findBlacks blacks)
        blacksNew = filter (turnBlack blacks) (findWhites blacks)

findBlacks :: Set Tile -> [Tile]
findBlacks = elems

findWhites :: Set Tile -> [Tile]
findWhites blacks = nub [n | p <- elems blacks, n <- nbs p, not (member n blacks)]

stayBlack :: Set Tile -> Tile -> Bool
stayBlack blacks tile = let count = countNbs blacks tile
                         in count == 1 || count == 2

turnBlack :: Set Tile -> Tile -> Bool
turnBlack blacks tile = countNbs blacks tile == 2

countNbs :: Set Tile -> Tile -> Int
countNbs blacks = length . filter (`member` blacks) . nbs

nbs :: Tile -> [Tile]
nbs tile = map (add tile . step) . enumFrom . toEnum $ 0
  where add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

blacks :: [[Dir]] -> Set Tile
blacks = S.fromList . foldl flipTile [] . map findTile

flipTile :: [Tile] -> Tile -> [Tile]
flipTile blacks tile = case tile `elem` blacks of
                            True -> filter (/= tile) blacks
                            False -> tile:blacks

findTile :: [Dir] -> Tile
findTile = foldl move (0, 0)
  where move (tx, ty) d = let (sx, sy) = step d
                           in (tx+sx, ty+sy)

step :: Dir -> (Int, Int)
step d = case d of
              E  -> (2, 0)
              SE -> (1, 1)
              SW -> (-1, 1)
              W  -> (-2, 0)
              NW -> (-1, -1)
              NE -> (1, -1)


parse :: String -> [[Dir]]
parse = map parseDirs . lines

parseDirs :: String -> [Dir]
parseDirs [] = []
parseDirs s = case s of
                   ('e':s')     -> E  : parseDirs s'
                   ('s':'e':s') -> SE : parseDirs s'
                   ('s':'w':s') -> SW : parseDirs s'
                   ('w':s')     -> W  : parseDirs s'
                   ('n':'w':s') -> NW : parseDirs s'
                   ('n':'e':s') -> NE : parseDirs s'
