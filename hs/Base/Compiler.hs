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

takeSymbol :: SexpParser a String
takeSymbol =
    takeSexp >>> (id ||| (arr (show >>> ("Symbol expected: "++)) >>> fail))

symbolMacro = skip . eq . symbol

takeNode :: SexpParser a [Sexp]
takeNode =
    takeSexp >>> ((arr (show >>> ("Node expected: "++)) >>> fail) ||| id)

looseParseNode :: SexpParser a b -> SexpParser a b
looseParseNode parseInner = applyParser takeNode parseInner

parseNode parseInner = looseParseNode (parseInner >>> empty)

looseMacro :: String -> SexpParser a b -> SexpParser a b
looseMacro name parseInner = 
    looseParseNode $ symbolMacro name >>> parseInner

macro :: String -> SexpParser a b -> SexpParser a b
macro name parseInner = looseMacro name (parseInner >>> empty)

testMacro :: (Compilable x a Code) => String -> SexpParser () a -> [(String, String)] -> IO ()
testMacro name mac testCases = testCompiler name ((toIO mac >>> compile) :: IOArrow [Sexp] Code) testCases

sexpCompiler :: (Compilable x a Code) => SexpParser () a -> IO ()
sexpCompiler mac = compiler (((Kleisli (\x -> (do putStrLn "here";return x))) >>> toIO mac >>> compile) :: IOArrow [Sexp] Code)
