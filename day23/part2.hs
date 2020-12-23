import Data.Map.Strict (Map, (!), union, insert)
import qualified Data.Map.Strict as M

size  = 1000000  :: Int
steps = 10000000 :: Int

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [Int] -> Int
solve ls = let p = m ! 1 in p * (m ! p)
  where start = (head ls, initMap ls)
        m = snd . head . drop steps . iterate step $ start

step :: (Int, Map Int Int) -> (Int, Map Int Int)
step (c, m) = (m' ! c, m')
  where n1 = m ! c
        n2 = m ! n1
        n3 = m ! n2
        d  = nextDest c [n1,n2,n3]
        dn = m ! d
        m' = insert c (m ! n3) . insert d n1 . insert n3 dn $ m

initMap :: [Int] -> Map Int Int
initMap ls = m1 `union` m2 `union` m3
  where m1 = M.fromList $ ls `zip` (tail ls)
        m2 = M.fromList $ [(last ls, 1 + length ls), (size, head ls)]
        m3 = M.fromList $ [1..size] `zip` [2..size]

nextDest :: Int -> [Int] -> Int
nextDest d tcups = nextDest' (decDest d)
  where nextDest' d' | d' `notElem` tcups = d'
                     | otherwise = nextDest' (decDest d')

decDest :: Int -> Int
decDest n | n == 1    = size
          | otherwise = n - 1

parse :: String -> [Int]
parse = map (\c -> read [c])
