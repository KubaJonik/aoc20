import Data.Function (on)
import Data.List (sort, groupBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.Map (Map, (!), findWithDefault)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: Map String [String] -> Int
solve gr = (length $ dfs gr "shiny gold" []) - 1

dfs :: Map String [String] -> String -> [String] -> [String]
dfs gr n vs | n `elem` vs = vs
            | otherwise   = dfs' (findWithDefault [] n gr) (n:vs)
  where dfs' [] vs' = vs'
        dfs' (x:xs) vs' = let nvs' = dfs gr x vs'
                            in dfs' xs nvs'

buildGraph :: [[String]] -> Map String [String]
buildGraph ss = Map.fromList . toGraph $ groupEdges (ss >>= toEdges)
  where toEdges (x:xs) = [(y,x) | y <- xs]
        groupEdges = groupBy ((==) `on` fst) . sort
        toGraph    = map (\s -> (fst . head $ s, map snd s))

parse :: String -> Map String [String]
parse = buildGraph . map parseLine . lines

parseLine :: String -> [String]
parseLine s = parseFirstBag ws : parseBags (drop 5 ws)
    where ws = words s

parseFirstBag :: [String] -> String
parseFirstBag ws = ws !! 0 ++ " " ++ ws !! 1

parseBags :: [String] -> [String]
parseBags (p:c:xs) = (p++" "++c) : parseBags (drop 2 xs)
parseBags _ = []
