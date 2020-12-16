import Data.List (transpose, isInfixOf)

data Rule = Rule {
    label  :: String,
    ranges :: [Range]
  } deriving (Show, Eq)


type Ticket = [Int]
type Range  = (Int, Int)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve (rules, myTicket, otherTickets) = product rs3
  where rs1 = reduce . findRules $ (rules, myTicket, otherTickets)
        rs2 = filter (\(_, (Rule lbl _)) -> "departure" `isInfixOf` lbl) rs1
        rs3 = map fst rs2


reduce :: [(Int, [Rule])] -> [(Int, Rule)]
reduce [] = []
reduce rules = let (r, rs) = red rules
                in r : reduce rs

red rs = (sr, rs2)
  where sr = head . map (\(a,b) -> (a, head b)) $ filter (\a -> length (snd a) == 1) rs
        rs1 = filter (\(a,_) -> a /= (fst sr)) rs
        rs2 = map (\(a, b) -> (a, filter (/=(snd sr)) b)) rs1

findRules :: ([Rule], Ticket, [Ticket]) -> [(Int, [Rule])]
findRules (rules, myTicket, otherTickets) = mts
  where vts  = validTickets rules otherTickets
        cols = transpose vts
        rls = map (\col -> validRule col rules) cols
        mts = zip myTicket rls

validRule :: [Int] -> [Rule] -> [Rule]
validRule vs rs = filter (validC vs) $ rs

validC :: [Int] -> Rule -> Bool
validC vs (Rule _ rs) = all (not.invalidV rs) vs

validTickets :: [Rule] -> [Ticket] -> [Ticket]
validTickets rls tts = filter (valid rls) tts

valid :: [Rule] -> Ticket -> Bool
valid rls tt = null $ notInRanges rs tt
  where rs = concatMap ranges rls

notInRanges :: [Range] -> Ticket -> [Int]
notInRanges rs tt = filter (invalidV rs) tt

invalidV :: [Range] -> Int -> Bool
invalidV rs v = all (notInRange v) rs

notInRange :: Int -> Range -> Bool
notInRange v (l,u) = v < l || v > u

parse :: String -> ([Rule], Ticket, [Ticket])
parse s = (parseRules s, parseMyTicket s, parseNearbyTickets s)

parseMyTicket :: String -> Ticket
parseMyTicket = parseTicket . head . drop 22 . lines

parseNearbyTickets :: String -> [Ticket]
parseNearbyTickets = map parseTicket . drop 25 . lines

parseRules :: String -> [Rule]
parseRules = take 20 . map parseRule . lines

parseRule :: String -> Rule
parseRule s = Rule lbl (parseRanges (tail rs))
  where (lbl, rs) = break (==':') s

parseRanges :: String -> [Range]
parseRanges s = let ws = words s
                 in [parseRange (ws !! 0), parseRange (ws !! 2)]

parseRange :: String -> (Int, Int)
parseRange s = let (l, h) = break (=='-') s
                in (read l, read (tail h))

parseTicket :: String -> Ticket
parseTicket s = read ("[" ++ s ++ "]")

