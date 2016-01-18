-- | https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

module Main where

import           Control.Monad
import           ErrorCheckingAndExceptions
import           Evaluation
import           Parser
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let evaled = liftM show $ readExpr (head args) >>= eval
  print $ readExpr (head args)
  putStrLn $ extractValue $ trapError evaled
