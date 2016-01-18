-- | https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

module Main where

import           Control.Monad
import           Evaluation
import           REPL
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ head args
    otherwise -> putStrLn "Program takes only 0 or 1 argument"
