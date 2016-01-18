{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

data Nat = O | S Nat

data Vec n a where
  Nil  :: Vec O a
  Cons :: a -> Vec n a -> Vec (S n) a
infixr 5 `Cons`    -- make `Cons` be *right*-associative``

abc :: Vec (S (S (S O))) Char
abc = 'a' `Cons` 'b' `Cons` 'c' `Cons` Nil

safeHead :: Vec (S n) a -> a
safeHead (Cons x _) = x
