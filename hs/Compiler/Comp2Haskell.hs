module Comp2Haskell where
import Prelude hiding (lines, words, take)
import Control.Arrow
import Util
import Arrows
import Sexp
import Parser
import Model

compQuote :: SexpParser Sexp
compQuote = macro "'" inner
    where inner                  = unquote <+> procSymbol <+> procNode
          procSymbol             = takeSymbol >>> arr quoteSymbol
          procNode               = compNode (takeSymbol &&& inners) >>> arr quoteNode
          inners                 = many (unquoteAll <+> (inner >>> arr (singleNode "List"))) >>> arr (namedNode "++")
          unquote                = macro "," take
          unquoteAll             = macro ",@" take
          quoteSymbol            = singleNode "symbol" . singleNode "Str" . symbol
          quoteNode (lbl, inner) = namedNode "namedNode" [singleNode "Str" (symbol lbl), inner]

comp2haskell :: SexpParser [Sexp]
comp2haskell = simpleTraverse [compQuote]

-- compMac :: SexpParser () Sexp
-- compMac = macro "mac" ((takeSymbol &&& take &&& optional compWhere) >>> buildMac)
--     where compWhere = undefined
--           buildMac (name, (body, whereClause)) =
--               Sexp "=" ([symbol name,
--                          Sexp "macro" [singleNode "Str" name,
--                                        body]]
--                         ++ whereClause)

-- comp2haskell = macro "compiler" ((takeSymbol &&& many compMac) >>> buildCompiler)
--     where buildcompiler (name, macs) = 
--               (Sexp "haskell" (Sexp "module" [name, 
--                                               Sexp "Prelude" [Sexp "hiding" [lines, words, take]],
--                                               symbol "Control.Arrow",
--                                               symbol "Util",
--                                               symbol "Arrows",
--                                               symbol "Sexp",
--                                               symbol "Parser",
--                                               symbol "Compiler"]
--                                : macs))
                               
                               
                                              
