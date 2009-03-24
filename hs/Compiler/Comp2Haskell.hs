module Comp2Haskell where
import Prelude hiding (lines, words, take)
import Control.Arrow
import Util
import Arrows
import Sexp
import Parser
import Compiler

compQuote :: SexpParser () Sexp
compQuote = macro "'" inner
    where inner                  = unquote <+> procSymbol <+> procNode
          procSymbol             = takeSymbol >>> arr quoteSymbol
          procNode               = takeAnySexp (arr snd &&& (voidArrow >>> inners)) >>> arr quoteSexp
          inners                 = many (unquoteAll <+> (inner >>> arr (singleNode "List"))) >>> arr (Sexp "++")
          unquote                = macro "," take
          unquoteAll             = macro ",@" take
          quoteSymbol            = singleNode "symbol" . singleNode "str" . symbol
          quoteSexp (lbl, inner) = Sexp "Sexp" [singleNode "str" (symbol lbl), inner]

compMac :: SexpParser () Sexp
compMac = macro "mac" ((takeSymbol &&& take &&& optional compWhere) >>> buildMac)
    where compWhere
    where buildMac (name, (body, whereClause)) =
              Sexp "=" ([symbol name,
                         Sexp "macro" [singleNode "str" name,
                                       body]]
                        ++ whereClause

compiler = macro "compiler" ((takeSymbol &&& many compMac) >>> buildCompiler)
    where buildcompiler (name, macs) = 
              (Sexp "haskell" (Sexp "module" [name, 
                                              Sexp "Prelude" [Sexp "hiding" [lines, words, take]],
                                              symbol "Control.Arrow",
                                              symbol "Util",
                                              symbol "Arrows",
                                              symbol "Sexp",
                                              symbol "Parser",
                                              symbol "Compiler"]
                               : macs))
                               
                               
                                              
