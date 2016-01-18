-- | https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

module Main where

import           Control.Monad
import           Data.Char                     (digitToInt, isDigit)
import           Data.Complex
import           Data.Ratio                    (denominator, numerator, (%))
import           Numeric                       (readDec, readFloat, readHex,
                                                readInt, readOct)
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~" <?> "a symbol"

-- readExpr :: String -> Either ParseError LispVal
-- readExpr = parse parseExpr "lisp"

-- Whitespace
spaces :: Parser ()
spaces = skipMany1 space

-- Return Values
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Character Char
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many (parseHelper <|> noneOf "\\\"")
  char '"'
  return $ String str
  <?> "a string for parseString"
  where parseHelper :: Parser Char
        parseHelper = do
          char '\\'
          x <- oneOf "\\\"nrt"
          return $ case x of
            '\\' -> x
            '"'  -> x
            'n'  -> '\n'
            'r'  -> '\r'
            't'  -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  return $ Atom (first : rest)
  <?> "a atom for parseAtom"

parseBool :: Parser LispVal
parseBool = do
  x <- try (string "#t") <|> string "#f"
  return $ if x == "#t"
           then Bool True
           else Bool False

parseNumber :: Parser LispVal
parseNumber = parseDigit <|> try parseOct <|> try parseDec <|> try parseHex <|> parseBin
  where parseOct = do
          char '#' >> oneOf "oO"
          str <- many1 octDigit
          let [(n, _)] = readOct str
          return $ Number n
        parseDec = do
          char '#' >> oneOf "dD"
          str <- many1 digit
          let [(n, _)] = readDec str
          return $ Number n
        parseHex = do
          char '#' >> oneOf "xX"
          str <- many1 hexDigit
          let [(n, _)] = readHex str
          return $ Number n
        parseBin = do
          char '#' >> oneOf "bB"
          str <- many1 $ oneOf "01"
          let [(n, _)] = readInt 2 (`elem` "01") digitToInt str
          return $ Number n
        parseDigit = do
          str <- many1 digit
          return $ (Number . read) str

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  x <- many $ letter <|> digit
  return $ case length x of
    0 -> Character '\n'
    1 -> Character $ head x
    _ -> case x of
      "newline" -> Character '\n'
      "tab"     -> Character '\t'
      "space"   -> Character ' '

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ let [(n, _)] = readFloat (x ++ '.' : y) in Float n

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio (read x % read y)

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseNumber
  char '+'
  y <- try parseFloat <|> parseNumber
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)
  where toDouble :: LispVal -> Double
        toDouble (Float f) = f
        toDouble (Number n) = fromIntegral n

-- Recursive Parsers: Adding lists, dotted lists, and quoted datums

parseList :: Parser LispVal
parseList = do
  char '('
  x <- sepBy parseExpr spaces
  char ')'
  return $ List x

parseDottedList :: Parser LispVal
parseDottedList = do
  char '('
  x <- endBy parseExpr spaces
  y <- char '.' >> spaces >> parseExpr
  char ')'
  return $ DottedList x y

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseBackquote :: Parser LispVal
parseBackquote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  y <- parseExpr
  return $ List [Atom "unquote", y]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  char ',' >> char '@'
  y <- parseExpr
  return $ List [Atom "unquote-splicing", y]

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> try parseBool
            <|> try parseRatio
            <|> try parseComplex
            <|> try parseFloat
            <|> try parseNumber
            <|> parseChar
            <|> parseString
            <|> try parseList
            <|> parseDottedList
            <|> parseQuoted
            <|> parseBackquote
            <|> try parseUnquoteSplicing
            <|> parseUnquote


-- Beginning the Evaluator
showVal :: LispVal -> String
showVal (Atom x) = x
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Ratio x) = show (numerator x) ++ ('/' : show (denominator x))
showVal (Complex x) = show (realPart x) ++ ('+' : show (imagPart x) ++ "i")
showVal (Float x) = show x
showVal (Number x) = show x
showVal (Character x) = "#\\" ++ show x
showVal (String x) = "\"" ++ x ++ "\""
showVal (List x) = "(" ++ unwordsList x ++ ")"
showVal (DottedList first rest)= "(" ++ unwordsList first ++ " . " ++ showVal rest ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

-- Beginnings of an evaluator: Primitives
main :: IO ()
main = getArgs >>= print . eval . readExpr . head

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Bool _) = val
eval val@(Ratio _) = val
eval val@(Complex _) = val
eval val@(Float _) = val
eval val@(Number _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val

-- Adding basic primitives
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("pair?", unaryOp pairp)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = Number $ foldl1 op $ map unpackNum args
  where unpackNum :: LispVal -> Integer
        unpackNum (Number n) = n
        unpackNum (String n) = let parsed = readDec n in
          if null parsed
          then 0
          else fst $ head parsed
        unpackNum (List [n]) = unpackNum n
        unpackNum _ = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op [x] = op x

symbolp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False

stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _ = Bool False

numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp _ = Bool False

boolp :: LispVal -> LispVal
boolp (Bool _) = Bool True
boolp _ = Bool False

listp :: LispVal -> LispVal
listp (List _) = Bool True
listp _ = Bool False

pairp :: LispVal -> LispVal
pairp (List _) = Bool True
pairp (DottedList _ _) = Bool True
pairp _ = Bool False
