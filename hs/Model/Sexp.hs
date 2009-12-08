{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
module Sexp where
import Prelude hiding (id, (.), take, lines, fail, Functor)
import Data.Char
import Control.Category
import Control.Arrow
import Control.Monad (liftM)
import System.Environment (getArgs)
import System.IO
import System.IO.Unsafe
import Util
import Arrows
import FailFunctor
import Code
import Parser
import Model

-- Basic defs and utils

data Sexp = Symbol { symbolName :: String }
          | Node   { children   :: [Sexp] }
            deriving (Eq)

type SexpParser a = IOParser Sexp () a
type LispMacro = SexpParser [Sexp]

sexp2either (Symbol s)   = Left s
sexp2either (Node sexps) = Right sexps

label :: Sexp -> String
label = symbolName . head . children

symbol = Symbol
node = Node
namedNode lbl xs = node ([symbol lbl] ++ xs)
singleNode lbl x = namedNode lbl [x]

isNode (Node _)   = True
isNode (Symbol _) = False

isSymbol (Node _)   = False
isSymbol (Symbol _) = True

symbolToLower (Symbol x) = Symbol (map toLower x)

labelEq name = (name==) . label

-- Reader

whitespace = skip $ many $ member " \t\n"
parseSym = many1 $ notMember " \t\n()"

parseParens p = skip (eq '(') >>> p >>> skip (eq ')')

parseSymbol = parseSym >>^ symbol

parseNode = parseParens parseSexps >>^ node

parseSexp = whitespace >>> (parseSymbol <+> parseNode) >>> whitespace

parseSexps = many parseSexp

instance Compilable (ExecFunParser Char Sexp) String Sexp where
    comp = parseSexp

readSexps :: IOArrow String [Sexp]
readSexps = compile  


-- Utils for parsing sexps

takeSexp = take >>^ sexp2either

takeSymbol =
    takeSexp >>> (id ||| (arr (show >>> ("Symbol expected: "++)) >>> fail))

compSymbol = takeSymbol >>^ symbol

instance Compilable (SexpParser String) [Sexp] String where
    comp = takeSymbol

symbolMacro = skip . eq . symbol

takeNode =
    takeSexp >>> ((arr (show >>> ("Node expected: "++)) >>> fail) ||| id)

looseCompNode compInner = applyParser takeNode compInner

compNode compInner = looseCompNode (compInner >>> empty)

looseMacro name compInner = 
    looseCompNode $ symbolMacro name >>> forceParser compInner

macro name compInner = looseMacro name (compInner >>> empty)

optMacro name compInner = macro name compInner <+> nilArrow

testMacro name mac testCases = testCompiler name ((toIO mac >>> compile) :: IOArrow [Sexp] Code) testCases

sexpCompiler mac = compiler ((toIO mac >>> compile) :: IOArrow [Sexp] Code)

-- gives a lisp-like macro expansion system
simpleTraverse :: [LispMacro] -> LispMacro
simpleTraverse macs = combinedMacs
    where combinedMacs = many (foldr (<+>) defaultMac recMacs) >>> arr concat
          recMacs      = map (\m -> applyParser m combinedMacs) macs
          defaultMac   = ((takeSymbol >>> arr (single . symbol)) <+> 
                          (compNode combinedMacs >>> arr (single . node)))

-- Pretty Print sexps

customLayoutSexp f =
    f <+> layoutSym <+> layoutNode
        where layoutSym  = takeSymbol >>^ text
              layoutNode = compNode (many (customLayoutSexp f) >>^ (parens . group . indent2 . lines))

customLayoutSexps f = many (customLayoutSexp f) >>^ paragraphs

layoutSexps :: SexpParser Code
layoutSexps = customLayoutSexps zeroArrow

customPrintSexps :: SexpParser Code -> SexpParser String
customPrintSexps f = customLayoutSexps f >>> arr comp

printSexps :: SexpParser String
printSexps = customPrintSexps zeroArrow

instance Compilable (SexpParser Code) [Sexp] Code where
    comp = layoutSexps

instance Show Sexp where
    show = unsafePerformIO . (runKleisli
                              (arr single >>>
                               toIO printSexps))
