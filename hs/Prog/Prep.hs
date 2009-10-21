module Prep where
import Control.Arrow
import Arrows
import Model
import Sexp
import Preprocessor

main = compiler (toIO preprocess)