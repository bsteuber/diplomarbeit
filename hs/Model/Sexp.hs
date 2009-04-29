{-# OPTIONS -fglasgow-exts #-}
module Sexp where
import Prelude hiding (id, (.), take, lines, fail, Functor)
import Control.Category
import Control.Arrow
import Control.Monad (liftM)
import System.Environment (getArgs)
import System.IO
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

type SexpParser a = ExecFunParser Sexp a

sexp2either (Symbol s)   = Left s
sexp2either (Node sexps) = Right sexps

label :: Sexp -> String
label = symbolName . head . children

symbol = Symbol
node = Node

isNode (Node _)   = True
isNode (Symbol _) = False

isSymbol (Node _)   = False
isSymbol (Symbol _) = True

labelEq name = (name==) . label

-- Reader

whitespace = skip $ many $ member " \t\n"
parseSym = many1 $ notMember " \t\n()"

parseParens p = skip (eq '(') >>> p >>> skip (eq ')')

parseSymbol = parseSym >>^ symbol

parseNode = parseParens parseSexps >>^ node

parseSexp = whitespace >>> (parseSymbol <+> parseNode) >>> whitespace

parseSexps = many parseSexp

instance Compilable (ExecFunParser Char [Sexp]) String [Sexp] where
    comp = parseSexps 

-- Utils for parsing sexps

takeSexp = take >>^ sexp2either

takeSymbol :: SexpParser String
takeSymbol =
    takeSexp >>> (id ||| (arr (show >>> ("Symbol expected: "++)) >>> fail))

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

testMacro name mac testCases = testCompiler name ((toIO mac >>> compile) :: IOArrow [Sexp] Code) testCases

sexpCompiler mac = compiler ((toIO mac >>> compile) :: IOArrow [Sexp] Code)

-- Pretty Print sexps

customLayoutSexp f =
    f <+> layoutSym <+> layoutNode
        where layoutSym  = takeSymbol >>^ text
              layoutNode = compNode (many (customLayoutSexp f) >>^ (parens . group . indent2 . lines))

customLayoutSexps f = many (customLayoutSexp f) >>^ paragraphs

layoutSexps :: SexpParser Code
layoutSexps = customLayoutSexps zeroArrow

customPrintSexps :: SexpParser Code -> SexpParser String
customPrintSexps f = customLayoutSexps f >>> lift comp

printSexps :: SexpParser String
printSexps = customPrintSexps zeroArrow

instance Compilable (SexpParser Code) [Sexp] Code where
    comp = layoutSexps

instance Show Sexp where
    show = execFail (execParser printSexps) . single
