module Comp2Haskell where
import Prelude hiding (lines, words, take)
import Control.Arrow
import Util
import Arrows
import Sexp
import Parser
import Compiler
import qualified Haskell2Code as H

quote :: SexpParser () Sexp
quote = macro "'" inner
    where inner                  = unquote <+> procSymbol <+> procNode
          procSymbol             = takeSymbol >>> arr quoteSymbol
          procNode               = takeAnySexp (arr snd &&& (voidArrow >>> inners)) >>> arr quoteSexp
          inners                 = many (unquoteAll <+> (inner >>> arr (singleNode "List"))) >>> arr (Sexp "++")
          unquote                = macro "," take
          unquoteAll             = macro ",@" take
          quoteSymbol            = singleNode "symbol" . singleNode "str" . symbol
          quoteSexp (lbl, inner) = Sexp "Sexp" [singleNode "str" (symbol lbl), inner]

compiler = macro "compiler"