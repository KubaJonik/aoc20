
data Action = N Int | S Int |
              E Int | W Int |
              L Int | R Int |
              F Int deriving Read

type Pos   = (Int, Int)
type State = (Pos, Pos)

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [Action] -> Int
solve = md . fst . foldl step initState

initState = ((0,0), (10,1)) :: State

md :: Pos -> Int
md (x, y) = abs x + abs y

step :: State -> Action -> State
step (pos,wp) ac = case ac of
                         N n -> (pos, move north n wp)
                         S n -> (pos, move south n wp)
                         E n -> (pos, move east n wp)
                         W n -> (pos, move west n wp)
                         F n -> (move wp n pos, wp)
                         L n -> (pos, turnLeft n wp)
                         R n -> (pos, turnRight n wp)

east  = (1, 0)  :: Pos
west  = (-1, 0) :: Pos
north = (0, 1)  :: Pos
south = (0, -1) :: Pos

move :: Pos -> Int -> Pos -> Pos
move (dx,dy) u (px,py) = (px+dx*u, py+dy*u)

turnLeft :: Int -> Pos -> Pos
turnLeft 0 p = p
turnLeft n (x,y) = turnLeft (n-90) (-y,x)

turnRight :: Int -> Pos -> Pos
turnRight 0 p = p
turnRight n (x,y) = turnRight (n-90) (y,-x)

parse :: String -> [Action]
parse = map parseLine . lines
  where parseLine (x:xs) = read ([x] ++ " " ++ xs)

