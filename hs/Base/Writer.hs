{-# OPTIONS -fglasgow-exts #-}
module Writer () where
import Prelude hiding (catch, lines, words)
import Control.Arrow
import Util
import Arrows
import Sexp
import Code
import Model

instance OfSexp Code where
    ofSexp = toIO sexp2code
        where sexp2code (Symbol name) = text name
              sexp2code (Node sexps)  = prin $ map sexp2code sexps
              prin = parens . group . indent2 . lines

instance ToString Sexp where
    toString = (ofSexp :: IOArrow Sexp Code) >>> toString