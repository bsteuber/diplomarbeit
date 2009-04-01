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
        where sexp2code (Sexp str stream) =
                  case stream of
                    [] -> text str
                    _  -> prin (text str : map sexp2code stream)
              prin = parens . group . (indent 2) . lines

instance ToString Sexp where
    toString = (ofSexp :: IOArrow Sexp Code) >>> toString