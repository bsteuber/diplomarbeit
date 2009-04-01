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

instance OfString Sexp where
    ofString = execParser (parseSexp >>> space >>> empty)

instance OfString [Sexp] where
    ofString = execParser (parseSexps >>> empty)
