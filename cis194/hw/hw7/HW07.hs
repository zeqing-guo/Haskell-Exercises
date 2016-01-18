{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import           Cards
import           Prelude              hiding (mapM)

import           Control.Monad        hiding (liftM, mapM)
import           Control.Monad.Random
import           Data.Functor
import           Data.Monoid
import           Data.Vector          (Vector, cons, (!), (!?), (//))
import           System.Random

import qualified Data.Vector          as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM  f a = do
  x <- a
  return $ f x

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = do
  x <- v !? i
  y <- v !? j
  let l = [(i, y), (j, x)]
  return $ v // l

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f (x : xs) = do
  y <- f x
  ys <- mapM f xs
  return $ y : ys

getElts :: [Int] -> Vector a -> Maybe [a]
getElts l v = mapM (v !?) l

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = do
  x <- getRandomR(0, V.length v)
  return $ v !? x

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec len = do
  l <- replicateM len getRandom
  return $ V.fromList l

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR len tp = do
  l <- replicateM len (getRandomR tp)
  return $ V.fromList l

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = do
  let ranges = [1..(V.length v - 1)]
  r <- mapM (\hi -> getRandomR (0, hi)) ranges
  let newV = foldl (\curV (x, y) -> swapUnsafe x y curV) v (zip ranges r)
  return newV
  where swapUnsafe :: Int -> Int -> Vector a -> Vector a
        swapUnsafe i j v' = let x = v' ! i
                                y = v' ! j
                                l = [(i, y), (j, x)]
                            in v' // l


-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v p = let ele = v ! p
                      low = V.take p v
                      high = V.drop (p + 1) v
                      newV = low V.++ high
                  in (V.filter (< ele) newV, ele, V.filter (>= ele) newV)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | V.null v = V.empty
  | otherwise = let (smaller, p, greater) = partitionAt v 0
                in qsort smaller V.++ V.cons p (qsort greater)

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | V.null v = return V.empty
  | otherwise = do
      p <- getRandomR (0, V.length v - 1)
      let (smaller, ele, greater) = partitionAt v p
      sortedSmaller <- qsortR smaller
      sortedGreater <- qsortR greater
      return $ sortedSmaller V.++ V.cons ele sortedGreater

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = V.fromList $ do
  x <- [Spade, Heart, Club, Diamond]
  y <- [Two .. Ace]
  return $ Card y x

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d = do
  h <- d !? 0
  let t = V.tail d
  return (h, t)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 d = Just ([], d)
getCards n d = do
  (card, deck) <- nextCard d
  (cards, deck') <- getCards (n - 1) deck
  return (card : cards, deck')

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
