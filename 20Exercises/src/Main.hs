-- | Zeqing Guo, 4th, August

module Main where

class Fluffy f where
    furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry _ Nothing = Nothing
  furry f (Just a) = Just (f a)

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = f . g

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left a)) = EitherLeft . Left $ f a
  furry _ (EitherLeft (Right t)) = EitherLeft . Right $ t

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry _ (EitherRight (Left t)) = EitherRight . Left $ t
  furry f (EitherRight (Right a)) = EitherRight . Right $ f a

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana = concatMap
  unicorn a = [a]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana _ Nothing = Nothing
  banana f (Just a) = f a
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f g t = f (g t) t
  unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left a)) = f a
  banana _ (EitherLeft (Right t)) = EitherLeft (Right t)
  unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana _ (EitherRight (Left t)) = EitherRight (Left t)
  banana f (EitherRight (Right a)) = f a
  unicorn = EitherRight . Right

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple x y = banana (\a ->
                      (banana (\b ->
                                 unicorn $ b a) y)) x

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy (x : xs) f = banana2 (:) (f x) (moppy xs f)

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage a = moppy a id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f a b = apple b (furry' f a)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f a b c = apple c (banana2 f a b)

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f a b c d = apple d (banana3 f a b c)

newtype State s a = State {
                            state :: s -> (s, a)
                          }

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f (State g) = State (\s0 -> let (s1, a) = g s0 in (s1, f a))

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  unicorn a = State (\s -> (s, a))
  banana f (State g) = State (\s0 ->
                                let (s1, a) = g s0
                                    State h = f a
                                    (s2, b) = h s1
                                in (s2, b))


main :: IO ()
main = putStrLn "hello world"

