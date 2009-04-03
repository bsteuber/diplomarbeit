{-# OPTIONS -fglasgow-exts #-}
module Reader () where
import Control.Arrow
import Sexp
import Arrows
import Parser
import Model

space = skip $ many $ member " \t\n"
parseSym = many1 $ notMember " \t\n()"

parens p = skip (eq '(') >>> p >>> skip (eq ')')

parseSymbol = parseSym >>^ symbol

parseNode = parens parseSexps >>^ node

parseSexp = space >>> (parseSymbol <+> parseNode)

parseSexps = many parseSexp >>> space

instance Parsable Char Sexp where
    parse = parseSexp >>> space

instance Compilable String Sexp where
    compile = execParser parse

instance Compilable String [Sexp] where
    compile = execParser parse
