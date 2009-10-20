module CMP2HS where
import Control.Arrow
import Arrows
import Model
import Sexp
import Code
import Comp2Haskell

main = compiler (toIO comp2haskell >>> (compile :: IOArrow [Sexp] Code))
