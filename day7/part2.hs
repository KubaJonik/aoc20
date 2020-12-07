import Data.List (isInfixOf)
import qualified Data.Map as Map
import Data.Map (Map, (!), findWithDefault)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: Map String [(Int, String)] -> Int
solve gr = (dfs gr "shiny gold") - 1

dfs :: Map String [(Int, String)] -> String -> Int
dfs gr n = 1 + sum [c * r | (c,n') <- ns, let r = dfs gr n']
    where ns = findWithDefault [] n gr

parse :: String -> Map String [(Int, String)]
parse = Map.fromList . map parseLine . filter (not.isInfixOf "other bags") . lines

parseLine :: String -> (String, [(Int, String)])
parseLine s = (parseFirstBag ws, parseBags (drop 4 ws))
    where ws = words s

parseFirstBag :: [String] -> String
parseFirstBag ws = ws !! 0 ++ " " ++ ws !! 1

parseBags :: [String] -> [(Int, String)]
parseBags (c:p:q:xs) = (read c, (p++" "++q)): parseBags (tail xs)
parseBags _ = []

