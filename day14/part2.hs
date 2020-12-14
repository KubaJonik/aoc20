import Data.Bits
import Data.List (isPrefixOf)
import Data.Map.Strict (Map, insert, elems)
import qualified Data.Map.Strict as M

data Bin = Z | O | F deriving Show

type Mask = [(Int, Bin)]
type Ass  = (Int, Int)
type Mem  = Map Int Int

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [(Mask, [Ass])] -> Int
solve = sum . elems . foldl updateMems M.empty

updateMems :: Mem -> (Mask, [Ass]) -> Mem
updateMems mem (mask, ass) = foldl (updateMem mask) mem ass

updateMem :: Mask -> Mem -> Ass -> Mem
updateMem mask mem (addr, val) = foldl (\me addr -> insert addr val me) mem addrs
  where addr' = applyMask1 mask addr
        masks = genMasks mask
        addrs = map (\ma -> applyMask2 ma addr') masks

applyMask1 :: Mask -> Int -> Int
applyMask1 mask val = foldl appBit val mask
    where appBit v (i, O) = setBit v i
          appBit v _      = v

applyMask2 :: Mask -> Int -> Int
applyMask2 mask val = foldl appBit val mask
    where appBit v (i, O) = setBit v i
          appBit v (i, Z) = clearBit v i

genMasks :: Mask -> [Mask]
genMasks = mapM (\(i,_) -> [(i,O), (i,Z)]) . filter isF
    where isF (_,F) = True
          isF _     = False

parse :: String -> [(Mask, [Ass])]
parse = map parseSegment . split . lines

parseSegment :: [String] -> (Mask, [Ass])
parseSegment ls = (parseMask (head ls), map parseAss (tail ls))

split :: [String] -> [[String]]
split [] = []
split (x:xs) = let (h, t) = span (not . isPrefixOf "mask") xs
                 in [x:h] ++ split t

parseMask :: String -> Mask
parseMask = map toBin . zip [0..] . reverse . drop 7
    where toBin (i,'0') = (i,Z)
          toBin (i,'1') = (i,O)
          toBin (i,'X') = (i,F)

parseAss :: String -> Ass
parseAss s = (read addr, read val)
    where addr = (takeWhile (/=']') . drop 4) s
          val  = (drop 2 . snd . break (=='=')) s
