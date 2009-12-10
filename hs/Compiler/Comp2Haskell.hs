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
    gen (name, lowerName) defs = ([node ([symbol "module"] ++ [name] ++ [node ([symbol "Prelude"] ++ [node ([symbol "hiding"] ++ [symbol "id"] ++ [symbol "lines"] ++ [symbol "words"] ++ [symbol "take"])])] ++ [symbol "Data.IORef"] ++ [node ([symbol "Control.Category"] ++ [node ([symbol "only"] ++ [symbol "id"])])] ++ [symbol "Control.Arrow"] ++ [symbol "System.IO.Unsafe"] ++ [symbol "Util"] ++ [symbol "Arrows"] ++ [symbol "Sexp"] ++ [symbol "Parser"] ++ [symbol "Model"])] ++ defs ++ [node ([symbol "hasType"] ++ [lowerName] ++ [symbol "LispMacro"])] ++ [node ([symbol "="] ++ [lowerName] ++ [node ([symbol "simpleTraverse"] ++ [node ([symbol "allAutoMacs"])])])])

compAllAutoMacs :: LispMacro

compAllAutoMacs = (macro "allAutoMacs" (getAutoMacs >>^ listAutoMacs))
  where
    listAutoMacs ms = ([node ([symbol "List"] ++ ms)])

compAutoMac :: LispMacro

compAutoMac = (macro "autoMac" (liftA4 gen (compSymbol >>> addAutoMac) compSymbol take (many take)))
  where
    gen fun sym cmd cmds = ([node ([symbol "hasType"] ++ [fun] ++ [symbol "LispMacro"])] ++ [node ([symbol "="] ++ [fun] ++ [node ([symbol "macro"] ++ [node ([symbol "Str"] ++ [sym])] ++ [cmd])] ++ cmds)])

compMac :: LispMacro

compMac = (macro "mac" (liftA4 gen compSymbol compSymbol take (many take)))
  where
    gen fun sym cmd cmds = ([node ([symbol "hasType"] ++ [fun] ++ [symbol "LispMacro"])] ++ [node ([symbol "="] ++ [fun] ++ [node ([symbol "macro"] ++ [node ([symbol "Str"] ++ [sym])] ++ [cmd])] ++ cmds)])

genQuotes :: LispMacro

genQuotes = (macro "quotes" (constArrow qts))
  where
    qts = (concatMap (genQt . mkSyms) ["", "1", "2", "3"])
    mkSyms s = (map (symbol . (\ str -> (str ++ s))) ["compQuote", "'", ",", ",@"])
    genQt [name, symQuote, symUnquote, symUnquoteAll] = ([node ([symbol "autoMac"] ++ [name] ++ [symQuote] ++ [symbol "inners"] ++ [node ([symbol "where"] ++ [node ([symbol "="] ++ [symbol "inner"] ++ [node ([symbol "<+>"] ++ [symbol "unquote"] ++ [symbol "procSymbol"] ++ [symbol "procNode"])])] ++ [node ([symbol "="] ++ [symbol "procSymbol"] ++ [node ([symbol ">>>"] ++ [symbol "takeSymbol"] ++ [node ([symbol "arr"] ++ [symbol "quoteSymbol"])])])] ++ [node ([symbol "="] ++ [symbol "procNode"] ++ [node ([symbol ">>>"] ++ [node ([symbol "compNode"] ++ [symbol "inners"])] ++ [node ([symbol "arr"] ++ [symbol "quoteNode"])])])] ++ [node ([symbol "="] ++ [symbol "inners"] ++ [node ([symbol ">>>"] ++ [node ([symbol "many"] ++ [node ([symbol "<+>"] ++ [symbol "unquoteAll"] ++ [node ([symbol ">>>"] ++ [symbol "inner"] ++ [node ([symbol "arr"] ++ [symbol "quoteList"])])])])] ++ [node ([symbol "arr"] ++ [symbol "concat"])] ++ [node ([symbol "arr"] ++ [symbol "quoteAppend"])])])] ++ [node ([symbol "mac"] ++ [symbol "unquote"] ++ [symUnquote] ++ [node ([symbol ">>^"] ++ [symbol "take"] ++ [symbol "single"])])] ++ [node ([symbol "mac"] ++ [symbol "unquoteAll"] ++ [symUnquoteAll] ++ [symbol "take"])] ++ [node ([symbol "="] ++ [node ([symbol "quoteSymbol"] ++ [symbol "sym"])] ++ [node ([symbol "'"] ++ [node ([symbol "Symbol"] ++ [node ([symbol "Str"] ++ [node ([symbol ","] ++ [node ([symbol "Symbol"] ++ [symbol "sym"])])])])])])] ++ [node ([symbol "="] ++ [node ([symbol "quoteNode"] ++ [symbol "sexps"])] ++ [node ([symbol "'"] ++ [node ([symbol "Node"] ++ [node ([symbol ",@"] ++ [symbol "sexps"])])])])] ++ [node ([symbol "="] ++ [node ([symbol "quoteList"] ++ [symbol "sexps"])] ++ [node ([symbol "'"] ++ [node ([symbol "List"] ++ [node ([symbol ",@"] ++ [symbol "sexps"])])])])] ++ [node ([symbol "="] ++ [node ([symbol "quoteAppend"] ++ [symbol "sexps"])] ++ [node ([symbol "'"] ++ [node ([symbol "++"] ++ [node ([symbol ",@"] ++ [symbol "sexps"])])])])])])])

compQuote :: LispMacro

compQuote = (macro "'" (inners >>^ single))
  where
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners = ((many (unquoteAll <+> (inner >>> (arr (namedNode "List"))))) >>> (arr (namedNode "++")))
    unquote :: LispMacro
    unquote = (macro "," (take >>^ single))
    unquoteAll :: LispMacro
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
    unquote :: LispMacro
    unquote = (macro ",1" (take >>^ single))
    unquoteAll :: LispMacro
    unquoteAll = (macro ",@1" take)
    quoteSymbol str = ([node ([symbol "symbol"] ++ [node ([symbol "Str"] ++ [symbol str])])])
    quoteNode nod = ([node ([symbol "node"] ++ [nod])])

compQuote2 :: LispMacro

compQuote2 = (macro "'2" (inners >>^ single))
  where
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners = ((many (unquoteAll <+> (inner >>> (arr (namedNode "List"))))) >>> (arr (namedNode "++")))
    unquote :: LispMacro
    unquote = (macro ",2" (take >>^ single))
    unquoteAll :: LispMacro
    unquoteAll = (macro ",@2" take)
    quoteSymbol str = ([node ([symbol "symbol"] ++ [node ([symbol "Str"] ++ [symbol str])])])
    quoteNode nod = ([node ([symbol "node"] ++ [nod])])

compQuote3 :: LispMacro

compQuote3 = (macro "'3" (inners >>^ single))
  where
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners = ((many (unquoteAll <+> (inner >>> (arr (namedNode "List"))))) >>> (arr (namedNode "++")))
    unquote :: LispMacro
    unquote = (macro ",3" (take >>^ single))
    unquoteAll :: LispMacro
    unquoteAll = (macro ",@3" take)
    quoteSymbol str = ([node ([symbol "symbol"] ++ [node ([symbol "Str"] ++ [symbol str])])])
    quoteNode nod = ([node ([symbol "node"] ++ [nod])])

comp2haskell :: LispMacro

comp2haskell = (simpleTraverse [compQuote3, compQuote2, compQuote1, compQuote, genQuotes, compMac, compAutoMac, compAllAutoMacs, compExpandCompiler])