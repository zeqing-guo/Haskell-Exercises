-- | Environment for scheme. https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Env
       ( Env
       , nullEnv
       , setVar
       , defineVar
       , getVar
       , bindVars
       ) where

import           Control.Monad.Error
import           Data.IORef
import           Data.Maybe                 (isJust)
import           ErrorCheckingAndExceptions
import           LispVal

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO addNewVar
  where addNewVar :: IO LispVal
        addNewVar = do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv :: [(String, LispVal)] -> [(String, IORef LispVal)] -> IO [(String, IORef LispVal)]
        extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding :: (String, LispVal) -> IO (String, IORef LispVal)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)
