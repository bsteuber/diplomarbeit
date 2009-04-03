{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances, IncoherentInstances #-}
module Compiler where
import Prelude hiding (id, (.), take, fail, Functor)
import Control.Category
import Control.Arrow
import Control.Monad (liftM)
import System.Environment (getArgs)
import System.IO
import Util
import Arrows
import Code
import Sexp
import Parser
import Reader
import Writer
import Model

type SexpParser = Parser Sexp

takeSexp :: SexpParser a (Either String [Sexp])
takeSexp = take >>^ sexp2either

instance Parsable Sexp String where
    parse = takeSymbol

takeSymbol :: SexpParser a String
takeSymbol =
    takeSexp >>> (id ||| (ioToParser compile >>> arr ("Symbol expected: "++) >>> fail))

symbolMacro = skip . eq . symbol

takeNode :: SexpParser a [Sexp]
takeNode =
    takeSexp >>> ((arr ("Node expected: "++) >>> fail) ||| id)

parseNode :: SexpParser a b -> SexpParser a b
parseNode parseInner = applyParser takeNode parseInner

looseMacro :: String -> SexpParser a b -> SexpParser a b
looseMacro name parseInner = 
    parseNode $ symbolMacro name >>> parseInner

macro :: String -> SexpParser a b -> SexpParser a b
macro name parseInner = looseMacro name (parseInner >>> empty)

-- compile :: SexpParser () a -> Sexp -> IO a
-- compile p sexp = execParser p [sexp]

testMacro :: (Compilable a Code) => String -> SexpParser () a -> [(String, String)] -> IO ()
testMacro name mac testCases = testCompiler name ((execParser mac >>> compile) :: IOArrow [Sexp] Code) testCases

sexpCompiler :: (Compilable a Code) => SexpParser () a -> IO ()
sexpCompiler mac = compiler ((execParser mac >>> compile) :: IOArrow [Sexp] Code)