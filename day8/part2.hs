import Control.Monad.State (State, get, put, runState)
import Data.Map.Strict (Map, fromList, (!), member, insert, size)
import Data.List (scanl)

data Status = Run | Loop | Halt
data Instr = Nop Int | Acc Int | Jmp Int

data Env = Env {
  acc     :: Int,
  idx     :: Int,
  instrs  :: Map Int Instr,
  visited :: [Int]
}

type EnvState = State Env

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [Env] -> Int
solve = acc . snd . head . filter fst . map (runState run)

run :: EnvState Bool
run = do
  instr <- currInstr
  case instr of
       Nop _ -> markVisited >> execNop
       Acc n -> markVisited >> execAcc n
       Jmp n -> markVisited >> execJmp n
  status <- getStatus
  case status of
       Halt -> return True
       Loop -> return False
       Run  -> run

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

getStatus :: EnvState Status
getStatus = do
  env <- get
  let sz = size (instrs env)
  let iv = idx env
  if (iv `elem` visited env) || iv < 1 || iv > sz+1 then return Loop
     else if iv-1 == sz then return Halt
     else return Run

currInstr :: EnvState Instr
currInstr = do
  env <- get
  return (instrs env ! idx env)

parse :: String -> [Env]
parse s = map (updateEnv env) idxs
  where instrs = parseInstrs s
        idxs   = swapIdxs instrs
        env    = initEnv instrs

updateEnv :: Env -> Int -> Env
updateEnv env idx = case (ins ! idx) of
                         Nop n -> env { instrs = insert idx (Jmp n) ins }
                         Jmp n -> env { instrs = insert idx (Nop n) ins }
                     where ins = instrs env

swapIdxs :: [(Int,Instr)] -> [Int]
swapIdxs = map fst . filter relOp
  where relOp (_, Nop _) = True
        relOp (_, Jmp _) = True
        relOp _          = False

initEnv :: [(Int, Instr)] -> Env
initEnv instr = Env 0 1 (fromList instr) []

parseInstrs :: String -> [(Int, Instr)]
parseInstrs = zip [1..] . map parseInstr . lines

parseInstr :: String -> Instr
parseInstr s = case words s of
                   ("nop":r) -> Nop . parseInt . head $ r
                   ("acc":r) -> Acc . parseInt . head $ r
                   ("jmp":r) -> Jmp . parseInt . head $ r

parseInt :: String -> Int
parseInt s = case s of
               ('+':r) -> read r
               ('-':r) -> -1 * read r

