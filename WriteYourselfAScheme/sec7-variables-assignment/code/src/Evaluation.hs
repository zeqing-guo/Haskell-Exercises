{-# LANGUAGE ExistentialQuantification #-}
-- | Evaluation module of scheme. https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/

module Evaluation
       ( evalAndPrint
       , runOne
       ) where

import           Control.Monad.Error
import           Env
import           ErrorCheckingAndExceptions
import           LispVal
import           Numeric                    (readDec)
import           Parser

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Bool _) = return val
eval env val@(Ratio _) = return val
eval env val@(Complex _) = return val
eval env val@(Float _) = return val
eval env val@(Number _) = return val
eval env val@(Character _) = return val
eval env (Atom var) = getVar env var
eval env (List [Atom "quote", val]) = return val

eval env (List [Atom "if", predicate, conseq, alt]) = do
  result <- eval env predicate
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    _ -> throwError $ TypeMismatch "bool" predicate

eval env (List (Atom "cond" : xs)) = cond env xs
eval env (List (Atom "case" : xs)) = schemeCase env xs

eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

-- Adding basic primitives
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env val = throwError $ BadSpecialForm "Unrecognized special form" val

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
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("pair?", unaryOp pairp),
              ("symbol->string", unaryOp symbol2String),
              ("string->symbol", unaryOp string2Symbol),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]
              -- ("cond", cond)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op args = liftM (Number . foldl1 op) (mapM unpackNum args)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = readDec n in
  if null parsed
  then throwError $ TypeMismatch "number" $ String n
  else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
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

-- Evaluation, Part 2
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise = do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool (left `op` right)

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- eqv for bool, number, string, atom, list and dotted list
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool x, Bool y] = return $ Bool $ x == y
eqv [Number x, Number y] = return $ Bool $ x == y
eqv [String x, String y] = return $ Bool $ x == y
eqv [Atom x, Atom y] = return $ Bool $ x == y
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List [x], List [y]] = eqv [x, y]
eqv [List (x : xs), List (y : ys)]
  | length xs /= length ys = return $ Bool False
  | otherwise = do
      Bool first <- eqv [x, y]
      Bool rest <- eqv [List xs, List ys]
      return $ Bool (first && rest)
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

-- equal
data Unpacker = forall a. Eq a => AnyUnpack (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpack unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return $ unpacked1 == unpacked2
  `catchError` (\_ -> return False) -- or (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [List [x], List [y]] = equal [x, y]
equal [List (x : xs), List (y : ys)]
  | length xs /= length ys = return $ Bool False
  | otherwise = do
      Bool first <- equal [x, y]
      Bool rest <- equal [List xs, List ys]
      return $ Bool $ first && rest
equal [DottedList head1 tail1, DottedList head2 tail2] = equal [List $ head1 ++ [tail1], List $ head2 ++ [tail2]]
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpack unpackNum, AnyUnpack unpackStr, AnyUnpack unpackBool]
  (Bool eqvEquals) <- eqv [arg1, arg2]
  return $ Bool $ primitiveEquals || eqvEquals

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env [List [Atom "else", x]] = eval env x
cond env [List [predicate, conseq]] = do
  Bool result <- eval env predicate
  if result
    then eval env conseq
    else throwError $ BadSpecialForm "no true clause in cond expression: " predicate
cond env (List [predicate, conseq] : xs) = do
  Bool result <- eval env predicate
  if result
    then eval env conseq
    else cond env xs
cond env form = throwError $ BadSpecialForm "ill-formed cond expression: " (List (Atom "cond" : form))

-- Right (List [Atom "case",List [Atom "*",Number 2,Number 3],List [List [Number 2,Number 3,Number 5,Number 7],List [Atom "quote",Atom "prime"]],List [List [Number 1,Number 4,Number 6,Number 8,Number 9],List [Atom "quote",Atom "composite"]]])
schemeCase :: Env -> [LispVal] -> IOThrowsError LispVal
schemeCase env (key : clauses) = do
  keyValue <- eval env key
  matchExpr keyValue clauses
  where matchExpr :: LispVal -> [LispVal] -> IOThrowsError LispVal
        matchExpr _ [List (Atom "else" : expr)] = evalAll expr
        matchExpr value [List (List datums : expr)] = if any eqvPair $ map (\x -> (value, x)) datums
                                                      then evalAll expr
                                                      else throwError $ CustomError "Unspecified return value"
        matchExpr value (List (List datums : expr) : others) = if any eqvPair $ map (\x -> (value, x)) datums
                                                               then evalAll expr
                                                               else matchExpr value others
        matchExpr _ badClause = throwError $ BadSpecialForm "Bad clause" (head badClause)
        eqvPair :: (LispVal, LispVal) -> Bool
        eqvPair (x1, x2) = case eqv [x1, x2] of
          Left _ -> False
          Right (Bool val) -> val
        evalAll :: [LispVal] -> IOThrowsError LispVal
        evalAll [x] = eval env x
        evalAll (x : xs) = eval env x `mplus` evalAll xs
        evalAll [] = throwError $ CustomError "Empty body"
