-- | Error checking and exceptions. https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/

module ErrorCheckingAndExceptions
       ( LispError(..)
       , ThrowsError
       , IOThrowsError
       , trapError
       , extractValue
       , liftThrows
       , runIOThrows)where

import           Control.Monad.Error
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Data.Complex
import           Data.Ratio                 (denominator, numerator)
import  {-# SOURCE #-} LispVal

-- Error Checking and Exceptions
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | CustomError String
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
showError (CustomError message)         = message

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Environment
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue $ runErrorT (trapError action)
