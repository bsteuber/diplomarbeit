module HS2C where
import Control.Arrow
import Arrows
import Model
import Sexp
import Haskell
import Code
import Sexp2Haskell
import Haskell2Code

main = compiler ((compile :: IOArrow [Sexp] Haskell) >>> 
                 (compile :: IOArrow Haskell Code))