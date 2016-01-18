-- | The parser for simple language

module Parser
       ( parser
       ) where

import           Com
import           Control.Monad
import           Data.Char
import           Exp

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) = f

instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f = Parser (\cs -> [(c, cs'') | (a, cs') <- parse p cs, (c, cs'') <- parse (f a) cs'])

instance MonadPlus Parser where
  mzero = Parser $ const []
  p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)

-- Get first element in a string
item :: Parser Char
item = Parser (\x -> case x of
                 "" -> []
                 (c : cs) -> [(c, cs)])

-- Deterministic choice of the first posibility of parsing.
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\xs -> case parse (p `mplus` q) xs of
                    [] -> []
                    (c : _) -> [c])

-- Using a predicate we can create a parser which is able to respect a specific rule
sat :: (Char -> Bool) -> Parser Char
sat f = do
  c <- item
  if f c then return c else mzero

-- A more general combinator
infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
p ? f = do
  c <- p
  if f c then return c else mzero

-- Function `char` is able to create the parser for a specific Char
char :: Char -> Parser Char
char c = sat (== c)

-- `many`: parser for a specific list
many :: Parser a -> Parser [a]
many p = (do
  c <- p
  cs <- many p
  return (c : cs)) +++ return []

-- Function `string` is able to create the parser for a specific String
string :: String -> Parser String
string "" = return ""
string (c : cs) = do
  char c
  string cs
  return (c : cs)
-- `space`: parser for some space
space :: Parser String
space = many $ sat isSpace

-- `token`: parser for a token followed by spaces
token :: Parser a -> Parser a
token p = do
  a <- p
  space
  return a

-- `symbol`: parser for a specific keyword followed by spaces
symbol :: String -> Parser String
symbol s = token $ string s

------------ Parser for variable ---------------

-- Parser for identifier
ident :: Parser String
ident = do
  first <- sat isAlpha
  rest <- many (sat (\c -> isAlpha c || isDigit c))
  return (first : rest)

-- Parser for identifier followed by spaces
identif :: Parser String
identif = do
  s <- ident
  space
  return s

-- Parser for variables
var :: Parser Exp
var = do
  v <- identif
  return $ Variable v

-- Parser for digit
digit :: Parser Exp
digit = do
  n <- many $ sat isDigit
  if n == "" then mzero else return $ Constant (read n :: Int)

----------- Parser for expressions -------------
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where
    rest a = (do f <- op
                 b <- p
                 rest $ f a b) +++ return a

rexp :: Parser Exp
rexp = expr `chainl1` relop

expr :: Parser Exp
expr = term `chainl1` addop

term :: Parser Exp
term = factor `chainl1` mulop

factor :: Parser Exp
factor = var +++ digit +++ do symbol "("
                              n <- rexp
                              symbol ")"
                              return n

addop :: Parser (Exp -> Exp -> Exp)
addop = do symbol "-"
           return Minus
        +++
        do symbol "+"
           return Plus

mulop :: Parser (Exp -> Exp -> Exp)
mulop = do symbol "*"
           return Times
        +++
        do symbol "/"
           return Divide

relop :: Parser (Exp -> Exp -> Exp)
relop = do symbol ">"
           return Greater
        +++
        do symbol "<"
           return Less
        +++
        do symbol "="
           return Equal

----------Parser for command ----------------
printe :: Parser Com
printe = do
  symbol "print"
  x <- rexp
  space
  return (Print x)

assign :: Parser Com
assign = do
  x <- identif
  symbol ":="
  e <- rexp
  space
  return (Assign x e)

seqv :: Parser Com
seqv = do
  symbol "{"
  c <- com
  symbol ";"
  d <- com
  symbol "}"
  return (Seq c d)

cond :: Parser Com
cond = do
  symbol "if"
  e <- rexp
  space
  symbol "then"
  c <- com
  symbol "else"
  d <- com
  return (Cond e c d)

while :: Parser Com
while = do
  symbol "while"
  e <- rexp
  space
  symbol "do"
  c <- com
  return (While e c)

declare :: Parser Com
declare = do
  symbol "declare"
  x <- identif
  symbol "="
  e <- rexp
  space
  symbol "in"
  c <- com
  return (Declare x e c)

com :: Parser Com
com = assign +++ seqv +++ cond +++ while +++ declare +++ printe

-- Parser
parser :: String -> Com
parser str = fst . head $ parse com str
