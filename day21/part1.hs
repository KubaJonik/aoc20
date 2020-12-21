import Data.Map.Strict (Map, elems, adjust, keys, (!))
import qualified Data.Map.Strict as M
import Data.List (nub, sortBy, groupBy, intersect, (\\))
import Data.Function (on)
import Data.Ord (comparing)

data Food = Food { ings :: [String], algs :: [String] }

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve fs = countIngs unings fs
  where uings  = findIngs [] (toAlgMap fs)
        aings  = nub . concatMap ings $ fs
        unings = aings \\ uings

countIngs :: [String] -> [Food] -> Int
countIngs is fs = sum [count i f | i <- is, f <- fs]
  where count i f = length . filter (==i) $ (ings f)

findIngs :: [String] -> Map String [Food] -> [String]
findIngs uings algMap | null algMap = uings
                      | otherwise   = findIngs uings' algMap'
                 where good    = findGood algMap
                       algMap' = reduce good algMap
                       uings'  = (ings good) ++ uings

reduce :: Food -> Map String [Food] -> Map String [Food]
reduce food algMap = foldl reduce' algMap' (keys algMap')
  where algMap' = foldl (flip M.delete) algMap (algs food)
        reduce' = flip (adjust (reduceFoods food))

reduceFoods :: Food -> [Food] -> [Food]
reduceFoods food = map (reduceFood food)

reduceFood :: Food -> Food -> Food
reduceFood food1 food2 = Food ings' algs'
  where ings' = (ings food2) \\ (ings food1)
        algs' = (algs food2) \\ (algs food1)

findGood :: Map String [Food] -> Food
findGood = head . filter good . map inter . elems
        
good :: Food -> Bool
good (Food i a) = length i == length a

inter :: [Food] -> Food
inter fs = Food ings' algs'
  where ings'  = inter' . map ings $ fs
        algs'  = inter' . map algs $ fs
        inter' = foldl1 intersect 

toAlgMap :: [Food] -> Map String [Food]
toAlgMap fs = M.fromList . mapFst . groupFst . sortFst . merge $ fs
  where toAlgList f = map (\a -> (a, f)) (algs f)
        groupFst    = groupBy ((==) `on` fst)
        sortFst     = sortBy (comparing fst)
        merge       = concatMap toAlgList
        mapFst      = map (\fs -> (fst . head $ fs, map snd fs))

parse :: String -> [Food]
parse = map parseFood . lines

parseFood :: String -> Food
parseFood s = Food ings' algs'
  where (l, r) = break (=='(') s
        ings'  = words l
        algs'  = words . init . drop 9 . filter (/=',') $ r

