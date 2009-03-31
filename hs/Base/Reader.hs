module Reader (readSexp) where
import Control.Arrow
import Sexp
import Arrows
import Parser

space = skip $ many $ member " \t\n"
parseSym = many1 $ notMember " \t\n()"

parens p = skip (eq '(') >>> p >>> skip (eq ')')

parseSymbol = parseSym >>^ symbol

parseNode = parens ((parseSym &&& parseSexps) >>^ uncurry Sexp)

parseSexp = space >>> (parseSymbol <+> parseNode) >>> space

parseSexps = many (space >>> parseSexp) >>> space

readSexp :: IOArrow String Sexp
readSexp = execParser (parseSexp >>> empty)

readSexps :: IOArrow String [Sexp]
readSexps = execParser (parseSexps >>> empty)
