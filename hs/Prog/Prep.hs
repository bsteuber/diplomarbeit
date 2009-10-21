module Prep where
import Control.Arrow
import Arrows
import Model
import Sexp
import Code
import Preprocessor

main = compiler (toIO preprocess >>> (compile :: IOArrow [Sexp] Code))