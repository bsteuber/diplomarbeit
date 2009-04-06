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

type SexpParser = Parser Sexp

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

instance Compilable (Parser Char () Sexp) String Sexp where
    comp = parseSexp 

-- Utils for parsing sexps

takeSexp :: SexpParser a (Either String [Sexp])
takeSexp = take >>^ sexp2either

takeSymbol :: SexpParser a String
takeSymbol =
    takeSexp >>> (id ||| (arr (show >>> ("Symbol expected: "++)) >>> fail))

symbolMacro = skip . eq . symbol

takeNode :: SexpParser a [Sexp]
takeNode =
    takeSexp >>> ((arr (show >>> ("Node expected: "++)) >>> fail) ||| id)

looseCompNode :: SexpParser a b -> SexpParser a b
looseCompNode compInner = applyParser takeNode compInner

compNode compInner = looseCompNode (compInner >>> empty)

looseMacro :: String -> SexpParser a b -> SexpParser a b
looseMacro name compInner = 
    looseCompNode $ symbolMacro name >>> compInner

macro :: String -> SexpParser a b -> SexpParser a b
macro name compInner = looseMacro name (compInner >>> empty)

testMacro :: (Compilable x a Code) => String -> SexpParser () a -> [(String, String)] -> IO ()
testMacro name mac testCases = testCompiler name ((toIO mac >>> compile) :: IOArrow [Sexp] Code) testCases

sexpCompiler :: (Compilable x a Code) => SexpParser () a -> IO ()
sexpCompiler mac = compiler (((Kleisli (\x -> (do putStrLn "here";return x))) >>> toIO mac >>> compile) :: IOArrow [Sexp] Code)

-- Pretty Print sexps

layoutSexp :: ExecParser Sexp Code -> ExecParser Sexp Code
layoutSexp f =
    f <+> layoutSym <+> layoutNode
        where layoutSym  = takeSymbol >>^ text
              layoutNode = compNode (many (layoutSexp f) >>^ (parens . group . indent2 . lines))

instance Compilable (ExecParser Sexp Code) [Sexp] Code where
    comp = layoutSexp zeroArrow

instance Compilable (Sexp -> String) Sexp String where
    comp = (comp :: Sexp -> Code) >>> comp

instance Show Sexp where
    show = comp