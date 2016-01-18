-- | https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

module Parser where

import           Control.Monad
import           Data.Char                     (digitToInt, isDigit)
import           Data.Complex
import           Data.Ratio                    ((%))
import           Numeric                       (readDec, readFloat, readHex,
                                                readInt, readOct)
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~" <?> "a symbol"

readExpr :: String -> Either ParseError LispVal
readExpr = parse parseExpr "lisp"

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
             deriving (Show)

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
  x <- parseExpr <|> parseQuasiquote <|> parseUnquote
  return $ List [Atom "quasiquote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  char ','
  y <- parseExpr
  return $ List [Atom "quasiquote", y]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ',' >> char '@'
  y <- parseExpr
  return $ List [Atom "unquote", y]

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
            <|> try parseUnquote
            <|> parseQuasiquote
