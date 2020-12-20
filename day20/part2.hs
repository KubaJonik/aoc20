import Control.Monad (guard)
import qualified Data.Set as S
import Data.Set (Set, member, elems)
import Data.Map.Strict (Map, keys, insert, (!))
import qualified Data.Map.Strict as M
import Data.List (nub, intersect, delete, groupBy, sort)
import Data.Bits

tileSize = 10 :: Int
size = 12     :: Int

data Tile = Tile {
  no     :: Int,
  grid   :: [(Int, Int)],
  left   :: Int,
  right  :: Int,
  top    :: Int,
  bottom :: Int
} deriving (Show, Eq, Ord)

main :: IO ()
main = do
  monster <- parseMonster <$> readFile "monster.txt"
  image   <- findImage    <$> readFile "input.txt"
  print $ countWaves monster image

countWaves :: [(Int,Int)] -> [(Int,Int)] -> Int
countWaves monster image = (length image) - (length taken)
  where taken = head . filter (not.null) . map (findMonsters monster) $ (imageSyms image)

findMonsters :: [(Int, Int)] -> [(Int,Int)] -> [(Int,Int)]
findMonsters monster image = nub . concatMap (findMonster imageS monster) $ ps
  where imageS = S.fromList image
        size' = tileSize * size - 2 * size
        ps    = [(x,y) | x <- [0..size'-1], y <- [0..size'-1]]

findMonster :: Set (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
findMonster image monster sp | all (flip member image) mpoints = mpoints
                             | otherwise = []
  where mpoints = map (addt sp) monster

addt :: (Int, Int) -> (Int, Int) -> (Int, Int)
addt (x1,y1) (x2,y2) = (x1+x2, y1+y2)

findImage :: String -> [(Int,Int)]
findImage = toImage . solveTiles . parse

toImage :: Map (Int, Int) Tile -> [(Int, Int)]
toImage m = concatMap (\k -> toPicture' k (grid (m ! k))) (keys m)
  where toPicture' (x,y) g = map (\(tx,ty) -> (x*tileSize'+tx, y*tileSize'+ty)) (trimGrid g)
        tileSize' = tileSize - 2

trimGrid :: [(Int,Int)] -> [(Int,Int)]
trimGrid = map (\(x,y) -> (x-1,y-1)) . filter notBorder
  where notBorder (x,y) = 0 < x && x < (tileSize - 1) && 0 < y && y < (tileSize - 1)

solveTiles :: [Tile] -> Map (Int, Int) Tile
solveTiles ats = head (solve' ats M.empty (0,0))
  where solve' [] grid _      = [grid]
        solve' lts grid (x,y) = do
                 tile <- findTiles (x,y) grid lts
                 let lts' = tile `delete` lts
                 sym <- syms tile
                 guard (check grid sym (x,y))
                 let grid' = insert (x,y) sym grid
                 solve' lts' grid' (nextXY (x,y))

        check _ _      (0, 0) = True
        check grid sym (x, y) | x == 0 && y > 0 = checkV (grid ! (x,y-1)) sym
                              | y == 0 && x > 0 = checkH (grid ! (x-1,y)) sym
                              | otherwise = (checkV (grid ! (x,y-1)) sym) && (checkH (grid ! (x-1,y)) sym)

        checkV t1 t2 = (bottom t1) == (top t2)
        checkH t1 t2 = (right t1) == (left t2)

        findTiles (0, 0) _ _      = t2
        findTiles (x, y) grid lts = inter (lts : findSibs (x,y) grid)

        findSibs (x, y) grid | x == 0 && y > 0 = [matchMap ! (no (grid ! (x,y-1)))]
                             | y == 0 && x > 0 = [matchMap ! (no (grid ! (x-1,y)))]
                             | otherwise = [matchMap ! (no (grid ! (x, y-1))), matchMap ! (no (grid ! (x-1,y)))] 

        matchList    = buildMatchList ats
        (t2, t3, t4) = groupMatches matchList
        matchMap     = M.fromList . map (\(t,ts) -> (no t, ts)) $ matchList

groupMatches :: [(Tile, [Tile])] -> ([Tile], [Tile], [Tile])
groupMatches ts = let gs = groupBy cfst . sort . map (\(t, ts) -> (length ts, t)) $ ts
                    in (map snd (gs !! 0), map snd (gs !! 1), map snd (gs !! 2))
                  where cfst (x,_) (y,_) = x == y

buildMatchList :: [Tile] -> [(Tile, [Tile])]
buildMatchList ts = map (\t -> (t, matches ts t)) ts

matches :: [Tile] -> Tile -> [Tile]
matches ts t = filter (match t) (t `delete` ts)

match :: Tile -> Tile -> Bool
match t1 t2 = any (match' t1) (syms t2)

match' :: Tile -> Tile -> Bool
match' t1 t2 = (left t1)   == (right t2)
            || (right t1)  == (left t2)
            || (top t1)    == (bottom t2)
            || (bottom t1) == (top t2)

imageSyms :: [(Int, Int)] -> [[(Int, Int)]]
imageSyms image = map ($ image) [t1,t2,t3,t4,t5,t6,t7,t8]
  where s  = size * tileSize - 2 * size
        t1 = id
        t2 = flipXGrid s
        t3 = flipYGrid s
        t4 = (flipXGrid s) . (flipYGrid s)
        t5 = (flipXGrid s) . (rotFGrid s)
        t6 = rotFGrid s
        t7 = rotBGrid s
        t8 = (flipXGrid s) . (rotBGrid s)

syms :: Tile -> [Tile]
syms tile = map ($ tile) [t1,t2,t3,t4,t5,t6,t7,t8]
  where t1 = id
        t2 = flipX
        t3 = flipY
        t4 = flipX . flipY
        t5 = flipX . rotF
        t6 = rotF
        t7 = rotB
        t8 = flipX . rotB

flipX :: Tile -> Tile
flipX t = t { grid = grid', left = left', right = right', top = top', bottom = bottom' }
  where left'   = right t
        right'  = left t
        top'    = flipN (top t)
        bottom' = flipN (bottom t)
        grid'   = flipXGrid tileSize (grid t)

flipY :: Tile -> Tile
flipY t = t { grid = grid', left = left', right = right', top = top', bottom = bottom' }
  where left'   = flipN (left t)
        right'  = flipN (right t)
        top'    = bottom t
        bottom' = top t
        grid'   = flipYGrid tileSize (grid t)

rotF :: Tile -> Tile
rotF t = t { grid = grid', left = left', right = right', top = top', bottom = bottom' }
  where left'   = bottom t
        right'  = top t
        top'    = flipN (left t)
        bottom' = flipN (right t)
        grid'   = rotFGrid tileSize (grid t)

rotB :: Tile -> Tile
rotB t = t { grid = grid', left = left', right = right', top = top', bottom = bottom' }
  where left'   = flipN (top t)
        right'  = flipN (bottom t)
        top'    = right t
        bottom' = left t
        grid'   = rotBGrid tileSize (grid t)

flipN :: Int -> Int
flipN n = flip' 0 0
  where flip' a b | b == tileSize = a
                  | testBit n b   = flip' (2*a+1) (b+1)
                  | otherwise     = flip' (2*a) (b+1)

rotFGrid :: Int -> [(Int, Int)] -> [(Int, Int)]
rotFGrid s = map (\(x,y) -> (s-1-y, x))

rotBGrid :: Int -> [(Int, Int)] -> [(Int, Int)]
rotBGrid s = map (\(x,y) -> (y, s-1-x))

flipXGrid :: Int -> [(Int, Int)] -> [(Int, Int)]
flipXGrid s = map (\(x,y) -> (s-1-x, y))

flipYGrid :: Int -> [(Int, Int)] -> [(Int, Int)]
flipYGrid s = map (\(x,y) -> (x, s-1-y))

-- Parsing -- 

parse :: String -> [Tile]
parse = parseTiles . lines

parseTiles :: [String] -> [Tile]
parseTiles [] = []
parseTiles l = parseTile (take (tileSize+1) l) : parseTiles (drop (tileSize+2) l)

parseTile :: [String] -> Tile
parseTile (ns:gs) = Tile no grid left right top bottom
  where no     = parseNo ns
        grid   = parseGrid gs
        left   = asBin . map snd . filter (\(x,_) -> x == 0) $ grid
        right  = asBin . map snd . filter (\(x,_) -> x == tileSize-1) $ grid
        top    = asBin . map fst . filter (\(_,y) -> y == 0) $ grid
        bottom = asBin . map fst . filter (\(_,y) -> y == tileSize - 1) $ grid

parseNo :: String -> Int
parseNo = read . init . drop 5

parseMonster :: String -> [(Int,Int)]
parseMonster = parseGrid . lines

parseGrid :: [String] -> [(Int,Int)]
parseGrid ls = concat $ [parseLine y l | (y, l) <- [0..] `zip` ls]
  where parseLine y l = [(x, y) | (x, c) <- [0..] `zip` l, c == '#']

inter :: (Eq a) => [[a]] -> [a]
inter (x:xs) = foldl intersect x xs

nextXY :: (Int, Int) -> (Int, Int)
nextXY (x,y) | x == (size-1) = (0, y+1)
             | otherwise     = (x+1,y)


asBin :: [Int] -> Int
asBin = sum . map (2^) . map ((tileSize-1) -)

toString :: Int -> [(Int,Int)] -> String
toString size grid = concatMap (toString' grid) [0..size-1]
  where toString' grid y = map (toChar grid y) [0..size-1] ++ "\n"
        toChar grid y x | (x,y) `elem` grid = '#'
                        | otherwise = '.'
