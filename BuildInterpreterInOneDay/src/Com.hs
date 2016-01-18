-- | Data structure for statements (commands)

module Com where

import           Exp

data Com = Assign String Exp
         | Seq Com Com
         | Cond Exp Com Com
         | While Exp Com
         | Declare String Exp Com
         | Print Exp
         deriving Show

