import Control.Monad
import Data.Char

data Expr = Con Int | Bin Op Expr Expr deriving Show
data Op   = Plus | Mul deriving Show

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: [Expr] -> Int
solve = sum . map eval

expr     = token (term >>= rest)
rest e1  = do { p <- mulop; e2 <- term; rest (Bin p e1 e2) } <|> return e1
term     = token (factor >>= more)
more e1  = do { p <- addop; e2 <- factor; more (Bin p e1 e2) } <|> return e1
factor   = token (constant <|> paren expr)
paren p  = do { symbol "("; x <- p; symbol ")"; return x }
addop    = symbol "+" >> return Plus
mulop    = symbol "*" >> return Mul
constant = nat >>= return . Con

eval :: Expr -> Int
eval (Con a) = a
eval (Bin op e1 e2) = let (v1, v2) = (eval e1, eval e2) in case op of
                        Plus  -> v1 + v2
                        Mul   -> v1 * v2

--- Parser ---
newtype Parser a = Parser (String -> [(a,String)])

parse :: String -> [Expr]
parse = map parseLine . lines

parseLine :: String -> Expr
parseLine = fst . head . apply expr

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return x = Parser (\s -> [(x,s)])
  p >>= q  = Parser (\s -> [(y,s'') | (x,s') <- apply p s, (y,s'') <- apply (q x) s'])

getc :: Parser Char
getc = Parser f where
  f []     = []
  f (c:cs) = [(c,cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
          c <- getc
          guardp (p c)
          return c

guardp :: Bool -> Parser ()
guardp True = return ()
guardp False = failp

failp :: Parser a
failp = Parser (const [])

char :: Char -> Parser ()
char x = do {c <- sat (==x); return () }

string :: String -> Parser ()
string []     = return ()
string (x:xs) = do { char x; string xs; return () }

digit = do { d <- sat isDigit; return (cvt d) }
  where cvt d = fromEnum d - fromEnum '0'

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f where
  f s = let ps = apply p s in
            if null ps then apply q s else ps

many p = do {x <- p; xs <- many p; return (x:xs) } <|> none
none = return []

space :: Parser ()
space = many (sat isSpace) >> return ()

token :: Parser a -> Parser a
token p = space >> p

symbol :: String -> Parser ()
symbol = token . string

some :: Parser a -> Parser [a]
some p = do {x <- p; xs <- many p; return (x:xs) }

natural :: Parser Int
natural = token nat
nat = do {ds <- some digit; return (foldl1 shift1 ds) } where
  shift1 m n = 10*m+n


