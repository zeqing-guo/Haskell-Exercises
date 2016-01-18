-- | https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

module Main where

import           Control.Monad
import           Control.Monad.Error
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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-- Beginnings of an evaluator: Primitives
-- main :: IO ()
-- main = getArgs >>= print . eval . readExpr . head
main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Bool _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval val@(Float _) = return val
eval val@(Number _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val

-- Adding basic primitives
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "No such function: " func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
              ("pair?", unaryOp pairp),
              ("symbol->string", unaryOp symbol2String),
              ("string->symbol", unaryOp string2Symbol)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op args = mapM unpackNum args >>= return . Number . foldl1 op
  where unpackNum :: LispVal -> ThrowsError Integer
        unpackNum (Number n) = return n
        -- unpackNum (String n) = let parsed = readDec n in
          -- if null parsed
          -- then 0
          -- else fst $ head parsed
        -- unpackNum (List [n]) = unpackNum n
        unpackNum notNum = throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [x] = op x
unaryOp _ wrongArg = throwError $ NumArgs 1 wrongArg

symbolp :: LispVal -> ThrowsError LispVal
symbolp (Atom _) = return $ Bool True
symbolp _ = return $ Bool False

stringp :: LispVal -> ThrowsError LispVal
stringp (String _) = return $ Bool True
stringp _ = return $ Bool False

numberp :: LispVal -> ThrowsError LispVal
numberp (Number _) = return $ Bool True
numberp _ = return $ Bool False

boolp :: LispVal -> ThrowsError LispVal
boolp (Bool _) = return $ Bool True
boolp _ = return $ Bool False

listp :: LispVal -> ThrowsError LispVal
listp (List _) = return $ Bool True
listp _ = return $ Bool False

pairp :: LispVal -> ThrowsError LispVal
pairp (List _) = return $ Bool True
pairp (DottedList _ _) = return $ Bool True
pairp _ = return $ Bool False

symbol2String :: LispVal -> ThrowsError LispVal
symbol2String (Atom x) = return $ String x
symbol2String mismatch = throwError $ TypeMismatch "atom" mismatch

string2Symbol :: LispVal -> ThrowsError LispVal
string2Symbol (String x) = return $ Atom x
string2Symbol mismatch = throwError $ TypeMismatch "string" mismatch

-- Error Checking and Exceptions
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                          ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val
