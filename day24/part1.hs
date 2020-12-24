
data Dir = E | SE | SW | W | NW | NE
type Tile = (Int, Int)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [[Dir]] -> Int
solve = length . foldl flipTile [] . map findTile

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
