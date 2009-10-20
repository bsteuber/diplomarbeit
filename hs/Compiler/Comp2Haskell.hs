module Comp2Haskell where
import Prelude hiding (lines, words, take)
import Control.Arrow
import Util
import Arrows
import Sexp
import Parser
import Model

compQuote :: LispMacro

compQuote = (macro "'" (inners >>^ single))
  where
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners = ((many (unquoteAll <+> (inner >>> (arr (namedNode "List"))))) >>> (arr (namedNode "++")))
    unquote = (macro "," (take >>^ single))
    unquoteAll = (macro ",@" take)
    quoteSymbol str = ([node ([symbol "symbol"] ++ [node ([symbol "Str"] ++ [symbol str])])])
    quoteNode nod = ([node ([symbol "node"] ++ [nod])])

compQuote1 :: LispMacro

compQuote1 = (macro "'1" (inners >>^ single))
  where
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners = ((many (unquoteAll <+> (inner >>> (arr (namedNode "List"))))) >>> (arr (namedNode "++")))
    unquote = (macro ",1" (take >>^ single))
    unquoteAll = (macro ",@1" take)
    quoteSymbol str = ([node ([symbol "symbol"] ++ [node ([symbol "Str"] ++ [symbol str])])])
    quoteNode nod = ([node ([symbol "node"] ++ [nod])])

comp2haskell :: LispMacro

comp2haskell = (simpleTraverse [compQuote, compQuote1])