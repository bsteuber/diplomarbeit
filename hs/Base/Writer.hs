{-# OPTIONS -fglasgow-exts #-}
module Writer () where
import Prelude hiding (catch, lines, words)
import Control.Arrow
import Util
import Arrows
import Sexp
import Code
import Model

instance Compilable Sexp Code where
    compile = toIO sexp2code
        where sexp2code (Symbol name) = text name
              sexp2code (Node sexps)  = prin $ map sexp2code sexps
              prin = parens . group . indent2 . lines

instance Compilable Sexp String where
    compile = (compile :: IOArrow Sexp Code) >>> compile