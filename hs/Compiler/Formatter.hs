module Formatter (format) where
import Prelude hiding (take)
import Arrows
import Parser
import Sexp

format :: ExecFunParser a [a]
format = many take