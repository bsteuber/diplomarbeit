module Comp2Haskell where
import Prelude hiding (lines, words)
import Arrows
import Sexp
import Parser
import Compiler

-- Utility

quoteSexp :: String -> [Sexp] -> Sexp
quoteSexp l cs =
    Sexp "Sexp" [Sexp "str" [symbol l],
                 Sexp "List" cs]
