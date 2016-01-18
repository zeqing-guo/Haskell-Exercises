-- | An simple Interpreter according to the paper "How to Build a Monadic Interpreter in One Day"

module Main where

import           Backend
import           Parser

main :: IO()
main = do
  let s = "declare x = 150 in declare y = 200 in {while x > 0 do {x := x - 1; y := y - 1}; print y}"
  print (interp $ parser s)
