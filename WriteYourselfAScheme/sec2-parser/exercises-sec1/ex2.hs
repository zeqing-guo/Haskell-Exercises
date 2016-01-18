-- | https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

module Parser where

import           Control.Monad
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" <?> "a symbol"

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
             | String String
             | Bool Bool
             deriving (Show)

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- parseHelper
  char '"'
  return $ String str
  <?> "a string for parseString"
  where parseHelper :: Parser String
        parseHelper = do
          x <- many $ noneOf "\"\\"
          y <- string "\\\"" <|> many (noneOf "\"")
          if x == ""
            then return ""
            else do z <- parseHelper
                    return (x ++ y ++ z)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom
  <?> "a atom for parseAtom"

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit) <?> "a number for parseNumber"

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseNumber
            <|> parseString

