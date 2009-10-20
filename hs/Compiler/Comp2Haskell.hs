module Comp2Haskell where
import Prelude hiding (lines, words, take)
import Control.Arrow
import Util
import Arrows
import Sexp
import Parser
import Model

compQuote :: LispMacro

compQuote = (macro "'" inners) >>^ single
  where
    inner = (unquote <+> unquoteAll <+> procSymbol <+> procNode)
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode = ((compNode  inners) >>> (arr quoteNode))
    inners = ((many (unquoteAll <+> (inner >>> (arr (singleNode "List"))))) >>> (arr (namedNode "++")))
    unquote = (macro "," take)
    unquoteAll = (macro ",@" take)
    quoteSymbol = ((singleNode "symbol") . (singleNode "Str") . symbol)
    quoteNode sexps = (namedNode "node" sexps)

comp2haskell :: LispMacro

comp2haskell = (simpleTraverse [compQuote])