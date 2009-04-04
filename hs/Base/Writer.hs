{-# OPTIONS -fglasgow-exts #-}
module Writer () where
import Prelude hiding (catch, lines, words)
import Control.Arrow
import Util
import Arrows
import Sexp
import Code
import Model

instance Compilable (Sexp -> Code) Sexp Code where
    comp (Symbol name) = text name
    comp (Node sexps)  = parens $ group $ indent2 $ lines $ map comp sexps

instance Compilable (Sexp -> String) Sexp String where
    comp = (comp :: Sexp -> Code) >>> comp

instance Show Sexp where
    show = comp