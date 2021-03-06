import Data.List.Split (splitOn)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [(Integer, Integer)] -> Integer
solve (x:xs) = fst (foldl step x xs)

step (ms, ds) (md, dd) = (m, ds*dd)
  where m = head . filter (\x -> x `mod` dd == ((dd - md) `mod` dd)) $ [ms, ms+ds..]

parse :: String -> [(Integer, Integer)]
parse s = map parse' (filter (\(_,y) -> y /= "x") ([0..] `zip` (splitOn "," . head . tail . lines $ s)))
  where parse' (x, y) = (x, read y)

