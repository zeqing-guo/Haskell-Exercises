-- | REPL model. https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL

module REPL
       ( runRepl
       ) where

import           Evaluation
import           System.IO

runRepl :: IO ()
runRepl = primitiveBindings >>= unitl_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

unitl_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
unitl_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> unitl_ pred prompt action

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


