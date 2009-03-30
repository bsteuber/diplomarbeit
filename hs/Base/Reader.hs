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

readSexp :: String -> IO Sexp
readSexp = execParser (parseSexp >>> empty)

readSexps = execParser (parseSexps >>> empty)
