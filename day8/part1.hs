import Control.Monad.State (State, get, put, execState)
import Control.Monad (unless)
import Data.Map.Strict (Map, fromList, (!))

data Instr = Nop | Acc Int | Jmp Int deriving Show

data Env = Env {
  acc     :: Int,
  idx     :: Int,
  instrs  :: Map Int Instr,
  visited :: [Int]
} deriving Show

type EnvState = State Env

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: Env -> Int
solve = acc . execState run

run :: EnvState ()
run = do
  instr <- currInstr
  case instr of
       Nop   -> markVisited >> execNop
       Acc n -> markVisited >> execAcc n
       Jmp n -> markVisited >> execJmp n
  halt <- shouldHalt
  unless halt run

execNop :: EnvState ()
execNop = incIdx

execAcc :: Int -> EnvState ()
execAcc n = do
  av <- getAcc
  putAcc (av + n)
  incIdx

execJmp :: Int -> EnvState ()
execJmp = addIdx

getAcc :: EnvState Int
getAcc = do
  env <- get
  return (acc env)

putAcc :: Int -> EnvState ()
putAcc n = do
  env <- get
  put (env { acc = n })

incIdx :: EnvState ()
incIdx = addIdx 1

addIdx :: Int -> EnvState ()
addIdx n = do
  env <- get
  let iv = idx env
  put (env { idx = iv + n })

markVisited :: EnvState ()
markVisited = do
  env <- get
  let iv = idx env
  put (env { visited = iv:(visited env) })

shouldHalt :: EnvState Bool
shouldHalt = do
  env <- get
  return (idx env `elem` visited env)

currInstr :: EnvState Instr
currInstr = do
  env <- get
  return (instrs env ! idx env)

parse :: String -> Env
parse s = Env 0 1 (parseInstrs s) []

parseInstrs :: String -> Map Int Instr
parseInstrs = fromList . zip [1..] . map parseInstr . lines

parseInstr :: String -> Instr
parseInstr s = case words s of
                   ("nop":r) -> Nop
                   ("acc":r) -> Acc . parseInt . head $ r
                   ("jmp":r) -> Jmp . parseInt . head $ r

parseInt :: String -> Int
parseInt s = case s of
               ('+':r) -> read r
               ('-':r) -> -1 * read r
