import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: (Integer, [Integer]) -> Integer
solve (ts, bs) = uncurry (*) . minimum . map (\b -> ((b * ceil ts b) - ts, b)) $ bs

ceil :: Integer -> Integer -> Integer
ceil a b = ceiling (fromInteger a / fromIntegral b)

parse :: String -> (Integer, [Integer])
parse s = (read . head $ ls, mapMaybe readMaybe . splitOn "," $ (ls !! 1))
  where ls = lines s

