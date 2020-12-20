import Prelude hiding (flip)
import Data.List (delete)
import Data.Bits

tileSize = 10 :: Int

data Tile = Tile {
  no     :: Int,
  left   :: Int,
  right  :: Int,
  top    :: Int,
  bottom :: Int
} deriving (Show, Eq)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve ts = product . map no . filter (twoMatches ts) $ ts

twoMatches ts t = let len = length . filter (match t) $ (t `delete` ts)
                    in len == 2

match :: Tile -> Tile -> Bool
match t1 t2 = any (match' t1) (syms t2)

match' :: Tile -> Tile -> Bool
match' t1 t2 = (left t1)   == (right t2)
            || (right t1)  == (left t2)
            || (top t1)    == (bottom t2)
            || (bottom t1) == (top t2)

syms :: Tile -> [Tile]
syms tile = [Tile n l r t b | (l1,r1)   <- [(left', right'), (right', left')],
                              (l2,r2)   <- [(l1, r1), (flip l1, flip r1)],
                              (t1,b1)   <- [(top', bottom'), (bottom', top')],
                              (t2,b2)   <- [(t1, b1), (flip t1, flip b1)],
                              (l,r,t,b) <- [(l2,r2,t2,b2),(t2,b2,l2,r2)] ]
  where (left', right') = (left tile, right tile)
        (top', bottom') = (top tile, bottom tile)
        n = no tile

flip :: Int -> Int
flip n = flip' 0 0
  where flip' a b | b == tileSize = a
                  | testBit n b   = flip' (2*a+1) (b+1)
                  | otherwise     = flip' (2*a) (b+1)

-- Parsing -- 

parse :: String -> [Tile]
parse = parseTiles . lines

parseTiles :: [String] -> [Tile]
parseTiles [] = []
parseTiles l = parseTile (take (tileSize+1) l) : parseTiles (drop (tileSize+2) l)

parseTile :: [String] -> Tile
parseTile (ns:gs) = Tile no left right top bottom
  where no     = parseNo ns
        grid   = parseGrid gs
        left   = asBin . map snd . filter (\(x,_) -> x == 0) $ grid
        right  = asBin . map snd . filter (\(x,_) -> x == tileSize-1) $ grid
        top    = asBin . map fst . filter (\(_,y) -> y == 0) $ grid
        bottom = asBin . map fst . filter (\(_,y) -> y == tileSize - 1) $ grid

parseNo :: String -> Int
parseNo = read . init . drop 5

parseGrid :: [String] -> [(Int,Int)]
parseGrid ls = concat $ [parseLine y l | (y, l) <- [0..] `zip` ls]
  where parseLine y l = [(x, y) | (x, c) <- [0..] `zip` l, c == '#']

asBin :: [Int] -> Int
asBin = sum . map (2^) . map ((tileSize-1) -)
