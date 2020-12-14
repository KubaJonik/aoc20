import Data.Bits
import Data.List (isPrefixOf)
import Data.Map.Strict (Map, insert, elems)
import qualified Data.Map.Strict as M

data Bin = Z | O deriving Show

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
updateMem mask mem (addr, val) = let val' = applyMask mask val
                                   in insert addr val' mem

applyMask :: Mask -> Int -> Int
applyMask mask val = foldl appBit val mask
    where appBit v (i, Z) = clearBit v i
          appBit v (i, O) = setBit v i

parse :: String -> [(Mask, [Ass])]
parse = map parseSegment . split . lines

parseSegment :: [String] -> (Mask, [Ass])
parseSegment ls = (parseMask (head ls), map parseAss (tail ls))

split :: [String] -> [[String]]
split [] = []
split (x:xs) = let (h, t) = span (not . isPrefixOf "mask") xs
                 in [x:h] ++ split t

parseMask :: String -> Mask
parseMask = map toBin . filter bit . zip [0..] . reverse . drop 7
    where bit (_,c)     = c /= 'X'
          toBin (i,'0') = (i,Z)
          toBin (i,'1') = (i,O)

parseAss :: String -> Ass
parseAss s = (read addr, read val)
    where addr = (takeWhile (/=']') . drop 4) s
          val  = (drop 2 . snd . break (=='=')) s
