-- | Data structrue for expressions

module Exp where

data Exp = Constant Int
         | Variable String
         | Plus Exp Exp
         | Minus Exp Exp
         | Greater Exp Exp
         | Times Exp Exp
         | Divide Exp Exp
         | Less Exp Exp
         | Equal Exp Exp
         deriving Show
