import Data.List (partition)
import Data.Map (Map, member,(!))
import qualified Data.Map as M

data Rule = C Char | OR [[Rule]]

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: (Rule, [String]) -> Int
solve (r, ws) =  length . filter (match r) $ ws

match :: Rule -> String -> Bool
match r s = any null (matchRule r s)

matchRule :: Rule -> String -> [String]
matchRule (C c) [] = []
matchRule (C c) (x:xs) | c == x    = [xs]
                       | otherwise = []
matchRule (OR ors) s = ors >>= (`matchRules` s)

matchRules :: [Rule] -> String -> [String]
matchRules [] _ = []
matchRules (r:rs) s = matchRules' (r:rs) s

matchRules' :: [Rule] -> String -> [String]
matchRules' [] s = [s]
matchRules' (r:rs) s = matchRule r s >>= matchRules' rs

parse :: String -> (Rule, [String])
parse s = let (l, w) = break (=="") . lines $ s
           in (parseRule0 l, tail w)

parseRule0 ls = rule 0
  where rule = (map rule' [0..] !!)
        rule' n | n `member` cr = C (cr ! n)
                | otherwise     = OR (map (map rule) (lr ! n))
        (cr, lr) = parseRules' ls

parseRules' ls = let (crs, lrs) = partition ('"' `elem`) ls
                  in (M.fromList (map parseCr crs), M.fromList (map parseLr lrs))

parseCr :: String -> (Int, Char)
parseCr s = let (nr, rs) = break (==':') s
             in (read nr, head . drop 3 $ rs)

parseLr :: String -> (Int, [[Int]])
parseLr s = let (nr, rs) = break (==':') s
             in (read nr, parseLr' rs)

parseLr' :: String -> [[Int]]
parseLr' s = let (r1, r2) = break (=="|") (words s)
              in [map read . tail $ r1, map read . filter (/="|") $ r2]
