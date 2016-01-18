module LispVal
       ( LispVal(..)
       , unwordsList
       ) where

data LispVal

unwordsList :: [LispVal] -> String


instance Show LispVal