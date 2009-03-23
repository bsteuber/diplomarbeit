module Preprocessor where
import Arrows
import Sexp
import Parser
import Compiler

compToken  = looseMacro "ignore" nilArrow <+> procSexp (second compStream)
compStream = many compToken >>> arr concat

preprocess = compStream >>> (arr unSingle)
