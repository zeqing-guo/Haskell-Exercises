-- | Backend of our simple interpreter

module Backend
       ( test
       , interp) where

import           Com
import           Exp

type Location = Int
type Index = [String]
type Stack = [Int]

-- Return the location of a variable in enviroment
position :: String -> Index -> Location
position name = pos 1
  where
    pos n (nm : nms) = if name == nm
                     then n
                     else pos (n + 1) nms

-- Return the value from specified location
fetch :: Location -> Stack -> Int
fetch 1 (v : _) = v
fetch n (_ : vs) = fetch (n - 1) vs

put :: Location -> Int -> Stack -> Stack
put 1 x (_ : vs) = x : vs
put n x (v : vs) = v : put (n - 1) x vs

newtype M a = StOut (Stack -> (a, Stack, String))

unStOut :: M a -> Stack -> (a, Stack, String)
unStOut (StOut f) = f

instance Monad M where
  return x = StOut (\n -> (x, n, ""))
  e >>= f = StOut (\n -> let (a, n1, s1) = unStOut e n
                             (b, n2, s2) = unStOut (f a) n1
                         in (b, n2, s1 ++ s2))

-- The monadic action capable of returning a value from the environment as
-- the main result of a computation
getfrom :: Location -> M Int
getfrom i = StOut (\ns -> (fetch i ns, ns, ""))

-- The monadic action capable of modifying the stack
write :: Location -> Int -> M ()
write i v = StOut (\ns -> ((), put i v ns, ""))

-- Stack push
push :: Int -> M ()
push x = StOut (\ns -> ((), x : ns, ""))

-- Stack pop
pop :: M ()
pop = StOut (\m -> let (_ : ns) = m
                 in ((), ns, ""))

-- Expressions evaluator
eval1 :: Exp -> Index -> M Int
eval1 exp index = case exp of
  Constant n -> return n
  Variable x -> let loc = position x index
                in getfrom loc
  Minus x y -> do {
    a <- eval1 x index;
    b <- eval1 y index;
    return (a - b)
    }
  Greater x y -> do {
    a <- eval1 x index;
    b <- eval1 y index;
    return (if a > b then 1 else 0)
    }
  Times x y -> do {
    a <- eval1 x index;
    b <- eval1 y index;
    return (a * b)
    }

-- Command interpreter
interpret1 :: Com -> Index -> M ()
interpret1 stmt index = case stmt of
  Assign name e -> let loc = position name index
                   in do {
                     v <- eval1 e index;
                     write loc v
                     }
  Seq s1 s2 -> do {
    interpret1 s1 index;
    interpret1 s2 index;
    return ()
    }
  Cond e s1 s2 -> do {
    x <- eval1 e index;
    if x == 1
    then interpret1 s1 index
    else interpret1 s2 index
    }
  While e b -> let loop () = do {
                     v <- eval1 e index;
                     if v == 0
                     then return ()
                     else do {
                       interpret1 b index;
                       loop ()
                       }
                     }
               in loop ()
  Declare nm e stmt2 -> do {
    v <- eval1 e index;
    push v;
    interpret1 stmt2 (nm : index);
    pop
    }
  Print e -> do {
    v <- eval1 e index;
    output v;
    }
  where
    output v = StOut (\n -> ((), n, show v))

-- For test
test :: Exp -> (Int, Stack, String)
test a = unStOut (eval1 a []) []

interp :: Com -> ((), Stack, String)
interp a = unStOut (interpret1 a []) []
