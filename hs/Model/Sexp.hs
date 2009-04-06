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
import Code
import Parser
import Model

-- Basic defs and utils

data Sexp = Symbol { symbolName :: String }
          | Node   { children   :: [Sexp] }
            deriving (Eq)

type SexpParser = ParseFunctor Sexp

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

instance Compilable (ExecFunParser Char Sexp) String Sexp where
    comp = parseSexp 

-- Utils for parsing sexps

takeSexp :: (ArrowChoice ar) => SexpParser ar a (Either String [Sexp])
takeSexp = take >>^ sexp2either

takeSymbol :: (ArrowChoice ar) => SexpParser ar a String
takeSymbol =
    takeSexp >>> (id ||| (arr (show >>> ("Symbol expected: "++)) >>> fail))

symbolMacro = skip . eq . symbol

takeNode :: (ArrowChoice ar) => SexpParser ar a [Sexp]
takeNode =
    takeSexp >>> ((arr (show >>> ("Node expected: "++)) >>> fail) ||| id)

looseCompNode :: (ArrowChoice ar) => SexpParser ar a b -> SexpParser ar a b
looseCompNode compInner = applyParser takeNode compInner

compNode compInner = looseCompNode (compInner >>> empty)

looseMacro :: String -> SexpParser ar a b -> SexpParser ar a b
looseMacro name compInner = 
    looseCompNode $ symbolMacro name >>> compInner

macro :: (ArrowChoice ar) => String -> SexpParser ar a b -> SexpParser ar a b
macro name compInner = looseMacro name (compInner >>> empty)

testMacro :: (ArrowChoice ar) => (Compilable x a Code) => String -> SexpParser ar () a -> [(String, String)] -> IO ()
testMacro name mac testCases = testCompiler name ((toIO mac >>> compile) :: IOArrow [Sexp] Code) testCases

sexpCompiler :: (ArrowChoice ar) => (Compilable x a Code) => SexpParser ar () a -> IO ()
sexpCompiler mac = compiler (((Kleisli (\x -> (do putStrLn "here";return x))) >>> toIO mac >>> compile) :: IOArrow [Sexp] Code)

-- Pretty Print sexps

layoutSexp :: (ArrowChoice ar) => ExecParser ar Sexp Code -> ExecParser ar Sexp Code
layoutSexp f =
    f <+> layoutSym <+> layoutNode
        where layoutSym  = takeSymbol >>^ text
              layoutNode = compNode (many (layoutSexp f) >>^ (parens . group . indent2 . lines))

instance Compilable (ExecFunParser Sexp Code) [Sexp] Code where
    comp = layoutSexp zeroArrow

instance Compilable (ExecFunParser Sexp String) [Sexp] String where
    comp = (comp :: ExecFunParser Sexp Code) >>> lift comp

instance Show Sexp where
    show = comp