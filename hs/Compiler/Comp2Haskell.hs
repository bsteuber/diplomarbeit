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

regMacs :: (IORef [Sexp])

regMacs = (unsafePerformIO (newIORef []))

addMac = (ioToParser (\ m -> (do
  (modifyIORef regMacs (\ ms -> (m : ms)))
  (return m))))

getMacs = (ioToParser (const (readIORef regMacs)))

compExpandCompiler :: LispMacro

compExpandCompiler = (macro "expandCompiler" (liftA2 gen (compSymbol >>> (id &&& (arr symbolToLower))) (many take)))
  where
    gen (name, lowerName) defs = ([node ([symbol "module"] ++ [name] ++ [node ([symbol "Prelude"] ++ [node ([symbol "hiding"] ++ [symbol "id"] ++ [symbol "lines"] ++ [symbol "words"] ++ [symbol "take"])])] ++ [symbol "Data.IORef"] ++ [node ([symbol "Control.Category"] ++ [node ([symbol "only"] ++ [symbol "id"])])] ++ [symbol "Control.Arrow"] ++ [symbol "System.IO.Unsafe"] ++ [symbol "Util"] ++ [symbol "Arrows"] ++ [symbol "Sexp"] ++ [symbol "Parser"] ++ [symbol "Model"])] ++ defs ++ [node ([symbol "type"] ++ [lowerName] ++ [symbol "LispMacro"])] ++ [node ([symbol "="] ++ [lowerName] ++ [node ([symbol "simpleTraverse"] ++ [node ([symbol "allMacs"])])])])

compAllMacs :: LispMacro

compAllMacs = (macro "allMacs" (getMacs >>^ listMacs))
  where
    listMacs ms = ([node ([symbol "List"] ++ ms)])

compMac :: LispMacro

compMac = (macro "mac" (liftA4 gen (compSymbol >>> addMac) compSymbol take (many take)))
  where
    gen fun sym cmd cmds = ([node ([symbol "type"] ++ [fun] ++ [symbol "LispMacro"])] ++ [node ([symbol "="] ++ [fun] ++ [node ([symbol "macro"] ++ [node ([symbol "Str"] ++ [sym])] ++ [cmd])] ++ cmds)])

genQuotes :: LispMacro

genQuotes = (macro "quotes" (constArrow qts))
  where
    qts = (concatMap (genQt . mkSyms) ["", "1", "2", "3"])
    mkSyms s = (map (symbol . (\ str -> (str ++ s))) ["compQuote", "'", ",", ",@"])
    genQt [name, symQuote, symUnquote, symUnquoteAll] = ([node ([symbol "mac"] ++ [name] ++ [symQuote] ++ [node ([symbol ">>^"] ++ [symbol "inners"] ++ [symbol "single"])] ++ [node ([symbol "where"] ++ [node ([symbol "="] ++ [symbol "inner"] ++ [node ([symbol "<+>"] ++ [symbol "unquote"] ++ [symbol "procSymbol"] ++ [symbol "procNode"])])] ++ [node ([symbol "="] ++ [symbol "procSymbol"] ++ [node ([symbol ">>>"] ++ [symbol "takeSymbol"] ++ [node ([symbol "arr"] ++ [symbol "quoteSymbol"])])])] ++ [node ([symbol "="] ++ [symbol "procNode"] ++ [node ([symbol ">>>"] ++ [node ([symbol "compNode"] ++ [symbol "inners"])] ++ [node ([symbol "arr"] ++ [symbol "quoteNode"])])])] ++ [node ([symbol "="] ++ [symbol "inners"] ++ [node ([symbol ">>>"] ++ [node ([symbol "many"] ++ [node ([symbol "<+>"] ++ [symbol "unquoteAll"] ++ [node ([symbol ">>>"] ++ [symbol "inner"] ++ [node ([symbol "arr"] ++ [node ([symbol "namedNode"] ++ [node ([symbol "Str"] ++ [symbol "List"])])])])])])] ++ [node ([symbol "arr"] ++ [node ([symbol "namedNode"] ++ [node ([symbol "Str"] ++ [symbol "++"])])])])])] ++ [node ([symbol "="] ++ [symbol "unquote"] ++ [node ([symbol "macro"] ++ [node ([symbol "Str"] ++ [symUnquote])] ++ [node ([symbol ">>^"] ++ [symbol "take"] ++ [symbol "single"])])])] ++ [node ([symbol "="] ++ [symbol "unquoteAll"] ++ [node ([symbol "macro"] ++ [node ([symbol "Str"] ++ [symUnquoteAll])] ++ [symbol "take"])])] ++ [node ([symbol "="] ++ [node ([symbol "quoteSymbol"] ++ [symbol "str"])] ++ [node ([symbol "'1"] ++ [node ([symbol "symbol"] ++ [node ([symbol "Str"] ++ [node ([symbol ",1"] ++ [node ([symbol "symbol"] ++ [symbol "str"])])])])])])] ++ [node ([symbol "="] ++ [node ([symbol "quoteNode"] ++ [symbol "nod"])] ++ [node ([symbol "'1"] ++ [node ([symbol "node"] ++ [node ([symbol ",1"] ++ [symbol "nod"])])])])])])])

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

compQuote2 :: LispMacro

compQuote2 = (macro "'2" (inners >>^ single))
  where
    inner = (unquote <+> procSymbol <+> procNode)
    procSymbol = (takeSymbol >>> (arr quoteSymbol))
    procNode = ((compNode inners) >>> (arr quoteNode))
    inners = ((many (unquoteAll <+> (inner >>> (arr (namedNode "List"))))) >>> (arr (namedNode "++")))
    unquote = (macro ",2" (take >>^ single))
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
    unquote = (macro ",3" (take >>^ single))
    unquoteAll = (macro ",@3" take)
    quoteSymbol str = ([node ([symbol "symbol"] ++ [node ([symbol "Str"] ++ [symbol str])])])
    quoteNode nod = ([node ([symbol "node"] ++ [nod])])

comp2haskell :: LispMacro

comp2haskell = (simpleTraverse [compQuote3, compQuote2, compQuote1, compQuote, genQuotes, compMac, compAllMacs, compExpandCompiler])