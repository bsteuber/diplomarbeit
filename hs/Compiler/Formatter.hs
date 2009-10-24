module Formatter (format) where
import Prelude hiding (take)
import Arrows
import Parser
import Sexp

format :: (Show a) => ExecFunParser a [a]
format = many take