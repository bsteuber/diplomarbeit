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

regAutoMacs :: (IORef [Sexp])

regAutoMacs = (unsafePerformIO (newIORef []))

addAutoMac = (ioToParser (\ m -> (do
  (modifyIORef regAutoMacs (\ ms -> (m : ms)))
  (return m))))

getAutoMacs = (ioToParser (const (readIORef regAutoMacs)))

compExpandCompiler :: LispMacro

compExpandCompiler = (macro "expandCompiler" (liftA2 gen (compSymbol >>> (id &&& (arr symbolToLower))) (many take)))
  where
    gen (name, lowerName) defs = ([Node ([Symbol "module"] ++ [name] ++ [Node ([Symbol "Prelude"] ++ [Node ([Symbol "hiding"] ++ [Symbol "id"] ++ [Symbol "lines"] ++ [Symbol "words"] ++ [Symbol "take"])])] ++ [Symbol "Data.IORef"] ++ [Node ([Symbol "Control.Category"] ++ [Node ([Symbol "only"] ++ [Symbol "id"])])] ++ [Symbol "Control.Arrow"] ++ [Symbol "System.IO.Unsafe"] ++ [Symbol "Util"] ++ [Symbol "Arrows"] ++ [Symbol "Sexp"] ++ [Symbol "Parser"] ++ [Symbol "Model"])] ++ defs ++ [Node ([Symbol "hasType"] ++ [lowerName] ++ [Symbol "LispMacro"])] ++ [Node ([Symbol "="] ++ [lowerName] ++ [Node ([Symbol "simpleTraverse"] ++ [Node ([Symbol "allAutoMacs"])])])])

compAllAutoMacs :: LispMacro

compAllAutoMacs = (macro "allAutoMacs" (getAutoMacs >>^ listAutoMacs))
  where
    listAutoMacs ms = ([Node ([Symbol "List"] ++ ms)])

compAutoMac :: LispMacro

compAutoMac = (macro "autoMac" (liftA4 gen (compSymbol >>> addAutoMac) compSymbol take (many take)))
  where
    gen fun sym cmd cmds = ([Node ([Symbol "hasType"] ++ [fun] ++ [Symbol "LispMacro"])] ++ [Node ([Symbol "="] ++ [fun] ++ [Node ([Symbol "macro"] ++ [Node ([Symbol "Str"] ++ [sym])] ++ [cmd])] ++ cmds)])

compMac :: LispMacro

compMac = (macro "mac" (liftA4 gen compSymbol compSymbol take (many take)))
  where
    gen fun sym cmd cmds = ([Node ([Symbol "hasType"] ++ [fun] ++ [Symbol "LispMacro"])] ++ [Node ([Symbol "="] ++ [fun] ++ [Node ([Symbol "macro"] ++ [Node ([Symbol "Str"] ++ [sym])] ++ [cmd])] ++ cmds)])

genQuotes :: LispMacro

genQuotes = (macro "quotes" (constArrow qts))
  where
    qts = (concatMap (genQt . mkSyms) ["", "1", "2", "3"])
    mkSyms s = (map (symbol . (\ str -> (str ++ s))) ["compQuote", "'", ",", ",@"])
    genQt [name, symQuote, symUnquote, symUnquoteAll] = ([Node ([Symbol "autoMac"] ++ [name] ++ [symQuote] ++ [Symbol "inners"] ++ [Node ([Symbol "where"] ++ [Node ([Symbol "hasType"] ++ [Symbol "inner"] ++ [Symbol "LispMacro"])] ++ [Node ([Symbol "="] ++ [Symbol "inner"] ++ [Node ([Symbol "<+>"] ++ [Symbol "unquote"] ++ [Symbol "procSymbol"] ++ [Symbol "procNode"])])] ++ [Node ([Symbol "hasType"] ++ [Symbol "procSymbol"] ++ [Symbol "LispMacro"])] ++ [Node ([Symbol "="] ++ [Symbol "procSymbol"] ++ [Node ([Symbol ">>>"] ++ [Symbol "takeSymbol"] ++ [Node ([Symbol "arr"] ++ [Symbol "quoteSymbol"])])])] ++ [Node ([Symbol "hasType"] ++ [Symbol "procNode"] ++ [Symbol "LispMacro"])] ++ [Node ([Symbol "="] ++ [Symbol "procNode"] ++ [Node ([Symbol ">>>"] ++ [Node ([Symbol "compNode"] ++ [Symbol "inners"])] ++ [Node ([Symbol "arr"] ++ [Symbol "quoteNode"])])])] ++ [Node ([Symbol "hasType"] ++ [Symbol "inners"] ++ [Symbol "LispMacro"])] ++ [Node ([Symbol "="] ++ [Symbol "inners"] ++ [Node ([Symbol ">>>"] ++ [Node ([Symbol "many"] ++ [Node ([Symbol "<+>"] ++ [Symbol "unquoteAll"] ++ [Node ([Symbol ">>>"] ++ [Symbol "inner"] ++ [Node ([Symbol "arr"] ++ [Symbol "quoteList"])])])])] ++ [Node ([Symbol "arr"] ++ [Symbol "concat"])] ++ [Node ([Symbol "arr"] ++ [Symbol "quoteAppend"])])])] ++ [Node ([Symbol "mac"] ++ [Symbol "unquote"] ++ [symUnquote] ++ [Node ([Symbol ">>^"] ++ [Symbol "take"] ++ [Symbol "single"])])] ++ [Node ([Symbol "mac"] ++ [Symbol "unquoteAll"] ++ [symUnquoteAll] ++ [Node ([Symbol ">>^"] ++ [Symbol "take"] ++ [Symbol "single"])])] ++ [Node ([Symbol "="] ++ [Node ([Symbol "quoteSymbol"] ++ [Symbol "sym"])] ++ [Node ([Symbol "'"] ++ [Node ([Symbol "Symbol"] ++ [Node ([Symbol "Str"] ++ [Node ([Symbol ","] ++ [Node ([Symbol "Symbol"] ++ [Symbol "sym"])])])])])])] ++ [Node ([Symbol "="] ++ [Node ([Symbol "quoteNode"] ++ [Symbol "sexps"])] ++ [Node ([Symbol "'"] ++ [Node ([Symbol "Node"] ++ [Node ([Symbol ",@"] ++ [Symbol "sexps"])])])])] ++ [Node ([Symbol "="] ++ [Node ([Symbol "quoteList"] ++ [Symbol "sexps"])] ++ [Node ([Symbol "'"] ++ [Node ([Symbol "List"] ++ [Node ([Symbol ",@"] ++ [Symbol "sexps"])])])])] ++ [Node ([Symbol "="] ++ [Node ([Symbol "quoteAppend"] ++ [Symbol "sexps"])] ++ [Node ([Symbol "'"] ++ [Node ([Symbol "++"] ++ [Node ([Symbol ",@"] ++ [Symbol "sexps"])])])])])])])

compQuote :: LispMacro

compQuote = (macro "'" inners)
  where
    inner :: LispMacro
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol :: LispMacro
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode :: LispMacro
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners :: LispMacro
    inners = ((many (unquoteAll <+> (inner >>> (arr quoteList)))) >>> (arr concat) >>> (arr quoteAppend))
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
    inner :: LispMacro
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol :: LispMacro
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode :: LispMacro
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners :: LispMacro
    inners = ((many (unquoteAll <+> (inner >>> (arr quoteList)))) >>> (arr concat) >>> (arr quoteAppend))
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
    inner :: LispMacro
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol :: LispMacro
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode :: LispMacro
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners :: LispMacro
    inners = ((many (unquoteAll <+> (inner >>> (arr quoteList)))) >>> (arr concat) >>> (arr quoteAppend))
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
    inner :: LispMacro
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol :: LispMacro
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode :: LispMacro
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners :: LispMacro
    inners = ((many (unquoteAll <+> (inner >>> (arr quoteList)))) >>> (arr concat) >>> (arr quoteAppend))
    unquote :: LispMacro
    unquote = (macro ",3" (take >>^ single))
    unquoteAll :: LispMacro
    unquoteAll = (macro ",@3" (take >>^ single))
    quoteSymbol sym = ([Node ([Symbol "Symbol"] ++ [Node ([Symbol "Str"] ++ [Symbol sym])])])
    quoteNode sexps = ([Node ([Symbol "Node"] ++ sexps)])
    quoteList sexps = ([Node ([Symbol "List"] ++ sexps)])
    quoteAppend sexps = ([Node ([Symbol "++"] ++ sexps)])

comp2haskell :: LispMacro

comp2haskell = (simpleTraverse [compQuote3, compQuote2, compQuote1, compQuote, genQuotes, compMac, compAutoMac, compAllAutoMacs, compExpandCompiler])