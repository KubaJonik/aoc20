
data Action = N Int | S Int |
              E Int | W Int |
              L Int | R Int |
              F Int deriving (Read, Show)

type Pos   = (Int, Int)
type State = (Pos, Pos)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [Action] -> Int
solve = md . fst . foldl step initState

initState = ((0,0), (1,0)) :: State

md :: Pos -> Int
md (x, y) = abs x + abs y

step :: State -> Action -> State
step (pos,dir) ac = case ac of
                         N n -> (move north n pos, dir)
                         S n -> (move south n pos, dir)
                         E n -> (move east n pos, dir)
                         W n -> (move west n pos, dir)
                         F n -> (move dir n pos, dir)
                         L n -> (pos, turnLeft n dir)
                         R n -> (pos, turnRight n dir)

east  = (1, 0)  :: Pos
west  = (-1, 0) :: Pos
north = (0, 1)  :: Pos
south = (0, -1) :: Pos

move :: Pos -> Int -> Pos -> Pos
move (dx,dy) u (px,py) = (px+dx*u, py+dy*u)

turnLeft :: Int -> Pos -> Pos
turnLeft 0 p = p
turnLeft n p = turnLeft (n-90) p'
  where p' | p == east = north
           | p == west = south
           | p == north = west
           | p == south = east


turnRight :: Int -> Pos -> Pos
turnRight 0 p = p
turnRight n p = turnRight (n-90) p'
  where p' | p == east = south
           | p == west = north
           | p == north = east
           | p == south = west

parse :: String -> [Action]
parse = map parseLine . lines
  where parseLine (x:xs) = read ([x] ++ " " ++ xs)

