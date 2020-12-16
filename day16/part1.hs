
data Rule = Rule {
    label  :: String,
    ranges :: [Range]
  } deriving Show


type Ticket = [Int]
type Range  = (Int, Int)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: ([Rule], Ticket, [Ticket]) -> Int
solve (rls, _, tts) = sum . concatMap (invalid rls) $ tts

invalid :: [Rule] -> Ticket -> [Int]
invalid rls tt = notInRanges rs tt
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
