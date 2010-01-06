module Comp2Haskell where
import Prelude hiding (id, lines, words, take)
import Data.IORef
import Control.Category (id)
import Control.Arrow
import System.IO.Unsafe
import Util
import Arrows
import Sexp
import Parser
import Model
import Data.Maybe

comp2haskell :: LispMacro

comp2haskell = (applyParser (liftA2 gen compAutoCompiler ((many (genQuotes <+> (take >>^ single))) >>^ concat)) compAuto)
  where
    compAutoCompiler = (macro "compiler" (compSymbol &&& (optMacro "imports" (many take))))
    extractAutoMacs defs = (mapMaybe extractAutoMac defs)
    extractAutoMac (Node (Symbol "autoMac" : name : _)) = (Just name)
    extractAutoMac _ = Nothing
    gen (name, imports) defs = ([Node ([Symbol "module"] ++ [name] ++ [Node ([Symbol "Prelude"] ++ [Node ([Symbol "hiding"] ++ [Symbol "id"] ++ [Symbol "lines"] ++ [Symbol "words"] ++ [Symbol "take"])])] ++ [Symbol "Data.IORef"] ++ [Node ([Symbol "Control.Category"] ++ [Node ([Symbol "only"] ++ [Symbol "id"])])] ++ [Symbol "Control.Arrow"] ++ [Symbol "System.IO.Unsafe"] ++ [Symbol "Util"] ++ [Symbol "Arrows"] ++ [Symbol "Sexp"] ++ [Symbol "Parser"] ++ [Symbol "Model"] ++ imports)] ++ defs ++ [Node ([Symbol "def"] ++ [Symbol "LispMacro"] ++ [Symbol "compAuto"] ++ [Node ([Symbol "lispTraverse"] ++ [Node ([Symbol "List"] ++ (extractAutoMacs defs))])])])

compMac :: LispMacro

compMac = (macro "mac" (liftA4 gen take take take (many take)))
  where
    gen fun sym cmd cmds = ([Node ([Symbol "def"] ++ [Symbol "LispMacro"] ++ [fun] ++ [Node ([Symbol "macro"] ++ [Node ([Symbol "Str"] ++ [sym])] ++ [cmd])] ++ cmds)])

compAutoMac :: LispMacro

compAutoMac = (macro "autoMac" (liftA4 gen take take take (many take)))
  where
    gen fun sym cmd cmds = ([Node ([Symbol "def"] ++ [Symbol "LispMacro"] ++ [fun] ++ [Node ([Symbol "macro"] ++ [Node ([Symbol "Str"] ++ [sym])] ++ [cmd])] ++ cmds)])

compDef :: LispMacro

compDef = (macro "def" (liftA3 gen take take (many take)))
  where
    gen typ name cmds = ([Node ([Symbol "hasType"] ++ [name] ++ [typ])] ++ [Node ([Symbol "="] ++ [name] ++ cmds)])

genQuotes :: LispMacro

genQuotes = (macro "quotes" (constArrow qts))
  where
    qts = (concatMap (genQt . mkSyms) ["", "1", "2", "3"])
    mkSyms s = (map (symbol . (\ str -> (str ++ s))) ["compQuote", "'", ",", ",@"])
    genQt [name, symQuote, symUnquote, symUnquoteAll] = ([Node ([Symbol "autoMac"] ++ [name] ++ [symQuote] ++ [Symbol "inners"] ++ [Node ([Symbol "where"] ++ [Node ([Symbol "="] ++ [Symbol "inner"] ++ [Node ([Symbol "<+>"] ++ [Symbol "unquote"] ++ [Node ([Symbol ">>^"] ++ [Symbol "takeSymbol"] ++ [Symbol "quoteSymbol"])] ++ [Node ([Symbol ">>^"] ++ [Node ([Symbol "compNode"] ++ [Symbol "inners"])] ++ [Symbol "quoteNode"])])])] ++ [Node ([Symbol "="] ++ [Symbol "inners"] ++ [Node ([Symbol ">>^"] ++ [Node ([Symbol "many"] ++ [Node ([Symbol "<+>"] ++ [Symbol "unquoteAll"] ++ [Node ([Symbol ">>^"] ++ [Symbol "inner"] ++ [Symbol "quoteList"])])])] ++ [Node ([Symbol ">>>"] ++ [Symbol "concat"] ++ [Symbol "quoteAppend"])])])] ++ [Node ([Symbol "mac"] ++ [Symbol "unquote"] ++ [symUnquote] ++ [Node ([Symbol ">>^"] ++ [Symbol "take"] ++ [Symbol "single"])])] ++ [Node ([Symbol "mac"] ++ [Symbol "unquoteAll"] ++ [symUnquoteAll] ++ [Node ([Symbol ">>^"] ++ [Symbol "take"] ++ [Symbol "single"])])] ++ [Node ([Symbol "="] ++ [Node ([Symbol "quoteSymbol"] ++ [Symbol "sym"])] ++ [Node ([Symbol "'"] ++ [Node ([Symbol "Symbol"] ++ [Node ([Symbol "Str"] ++ [Node ([Symbol ","] ++ [Node ([Symbol "Symbol"] ++ [Symbol "sym"])])])])])])] ++ [Node ([Symbol "="] ++ [Node ([Symbol "quoteNode"] ++ [Symbol "sexps"])] ++ [Node ([Symbol "'"] ++ [Node ([Symbol "Node"] ++ [Node ([Symbol ",@"] ++ [Symbol "sexps"])])])])] ++ [Node ([Symbol "="] ++ [Node ([Symbol "quoteList"] ++ [Symbol "sexps"])] ++ [Node ([Symbol "'"] ++ [Node ([Symbol "List"] ++ [Node ([Symbol ",@"] ++ [Symbol "sexps"])])])])] ++ [Node ([Symbol "="] ++ [Node ([Symbol "quoteAppend"] ++ [Symbol "sexps"])] ++ [Node ([Symbol "'"] ++ [Node ([Symbol "++"] ++ [Node ([Symbol ",@"] ++ [Symbol "sexps"])])])])])])])

compQuote :: LispMacro

compQuote = (macro "'" inners)
  where
    inner = (unquote <+> (takeSymbol >>^ quoteSymbol) <+> ((compNode inners) >>^ quoteNode))
    inners = ((many (unquoteAll <+> (inner >>^ quoteList))) >>^ (concat >>> quoteAppend))
    unquote :: LispMacro
    unquote = (macro "," (take >>^ single))
    unquoteAll :: LispMacro
    unquoteAll = (macro ",@" (take >>^ single))
    quoteSymbol sym = ([Node ([Symbol "Symbol"] ++ [Node ([Symbol "Str"] ++ [Symbol sym])])])
    quoteNode sexps = ([Node ([Symbol "Node"] ++ sexps)])
    quoteList sexps = ([Node ([Symbol "List"] ++ sexps)])
    quoteAppend sexps = ([Node ([Symbol "++"] ++ sexps)])

compQuote1 :: LispMacro

compQuote1 = (macro "'1" inners)
  where
    inner = (unquote <+> (takeSymbol >>^ quoteSymbol) <+> ((compNode inners) >>^ quoteNode))
    inners = ((many (unquoteAll <+> (inner >>^ quoteList))) >>^ (concat >>> quoteAppend))
    unquote :: LispMacro
    unquote = (macro ",1" (take >>^ single))
    unquoteAll :: LispMacro
    unquoteAll = (macro ",@1" (take >>^ single))
    quoteSymbol sym = ([Node ([Symbol "Symbol"] ++ [Node ([Symbol "Str"] ++ [Symbol sym])])])
    quoteNode sexps = ([Node ([Symbol "Node"] ++ sexps)])
    quoteList sexps = ([Node ([Symbol "List"] ++ sexps)])
    quoteAppend sexps = ([Node ([Symbol "++"] ++ sexps)])

compQuote2 :: LispMacro

compQuote2 = (macro "'2" inners)
  where
    inner = (unquote <+> (takeSymbol >>^ quoteSymbol) <+> ((compNode inners) >>^ quoteNode))
    inners = ((many (unquoteAll <+> (inner >>^ quoteList))) >>^ (concat >>> quoteAppend))
    unquote :: LispMacro
    unquote = (macro ",2" (take >>^ single))
    unquoteAll :: LispMacro
    unquoteAll = (macro ",@2" (take >>^ single))
    quoteSymbol sym = ([Node ([Symbol "Symbol"] ++ [Node ([Symbol "Str"] ++ [Symbol sym])])])
    quoteNode sexps = ([Node ([Symbol "Node"] ++ sexps)])
    quoteList sexps = ([Node ([Symbol "List"] ++ sexps)])
    quoteAppend sexps = ([Node ([Symbol "++"] ++ sexps)])

compQuote3 :: LispMacro

compQuote3 = (macro "'3" inners)
  where
    inner = (unquote <+> (takeSymbol >>^ quoteSymbol) <+> ((compNode inners) >>^ quoteNode))
    inners = ((many (unquoteAll <+> (inner >>^ quoteList))) >>^ (concat >>> quoteAppend))
    unquote :: LispMacro
    unquote = (macro ",3" (take >>^ single))
    unquoteAll :: LispMacro
    unquoteAll = (macro ",@3" (take >>^ single))
    quoteSymbol sym = ([Node ([Symbol "Symbol"] ++ [Node ([Symbol "Str"] ++ [Symbol sym])])])
    quoteNode sexps = ([Node ([Symbol "Node"] ++ sexps)])
    quoteList sexps = ([Node ([Symbol "List"] ++ sexps)])
    quoteAppend sexps = ([Node ([Symbol "++"] ++ sexps)])

compAuto :: LispMacro

compAuto = (lispTraverse [compMac, compAutoMac, compDef, compQuote, compQuote1, compQuote2, compQuote3])