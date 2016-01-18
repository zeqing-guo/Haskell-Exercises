-- | Data structure of compiler. https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/

module LispVal
       ( LispVal(..)
       , Env
       , unwordsList
       ) where

import           Data.Complex
import           Data.IORef
import           Data.Ratio                 (denominator, numerator)
import           ErrorCheckingAndExceptions

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++ ") ...)"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

type Env = IORef [(String, IORef LispVal)]
