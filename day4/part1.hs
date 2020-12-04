
labels :: [String]
labels = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main :: IO ()
main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve = length . filter valid . parse . lines

valid :: [String] -> Bool
valid ls = all (`elem` ls) labels

parse :: [String] -> [[String]]
parse s = case span (/="") s of
            (h, []) -> [parsePass h]
            (h, s') -> parsePass h : parse (tail s')

parsePass :: [String] -> [String]
parsePass = map (take 3) . concatMap words
