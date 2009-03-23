module Formatter (format) where
import Prelude hiding (take)
import Arrows
import Sexp
import Parser
import Compiler

format = take