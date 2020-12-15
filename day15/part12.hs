import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap, insert, member, (!))

start = [18,11,9,0,5,1] :: [Int]

type State = (HashMap Int Int, (Int, Int))

main :: IO ()
main = print solve

solve :: Int
solve = ns . head . dropWhile sm . iterate next $ startState
  where sm (_, (_, t)) = t < 30000000
        ns (_, (n, _)) = n

next :: State -> State
next (m, (n, t)) | n `member` m = (insert n t m, (t - m ! n, t+1))
                 | otherwise    = (insert n t m, (0, t+1))

startState :: State
startState = let ns = start `zip` [1..]
              in (M.fromList . init $ ns, last ns)
