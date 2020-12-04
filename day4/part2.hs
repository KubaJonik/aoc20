import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.Read (readMaybe)

type Pass = [(String, String)]

labels :: [String]
labels = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main :: IO ()
main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve = length . filter valid . parse . lines

valid :: Pass -> Bool
valid p = labelsPresent p && all labelsValid p

labelsPresent :: Pass -> Bool
labelsPresent ls = all (`elem` (map fst ls)) labels

labelsValid :: (String, String) -> Bool
labelsValid ("byr",txt) = byrValid txt
labelsValid ("iyr",txt) = iyrValid txt
labelsValid ("eyr",txt) = eyrValid txt
labelsValid ("hgt",txt) = hgtValid txt
labelsValid ("hcl",txt) = hclValid txt
labelsValid ("ecl",txt) = eclValid txt
labelsValid ("pid",txt) = pidValid txt
labelsValid ("cid",txt) = True

byrValid :: String -> Bool
byrValid = isBetween 1920 2002

iyrValid :: String -> Bool
iyrValid = isBetween 2010 2020

eyrValid :: String -> Bool
eyrValid = isBetween 2020 2030

hgtValid :: String -> Bool
hgtValid s | ln < 3 = False
           | (drop (ln-2) s) == "cm" = isBetween 150 193 (take (ln-2) s)
           | (drop (ln-2) s) == "in" = isBetween 59 76 (take (ln-2) s)
           | otherwise = False
           where ln = length s

hclValid :: String -> Bool
hclValid s = length s == 7 && s =~ "#[0-9a-f]+"

eclValid :: String -> Bool
eclValid s = any (==s) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pidValid :: String -> Bool
pidValid s = length s == 9 && s =~ "[0-9]+"

isBetween :: Int -> Int -> String -> Bool
isBetween l u s = case (readMaybe s) of
                    Just n  -> l <= n && n <= u
                    Nothing -> False

parse :: [String] -> [Pass]
parse s = case span (/="") s of
            (h, []) -> [parsePass h]
            (h, s') -> parsePass h : parse (tail s')

parsePass :: [String] -> [(String, String)]
parsePass = map (\s -> (take 3 s, drop 4 s)) . concatMap words
