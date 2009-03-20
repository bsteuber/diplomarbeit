module Reader (readSexp, readSexps) where 

import Prelude hiding (catch)
import Control.Monad
import qualified Control.Exception as E
import Util
import Sexp
import Eater

type CharParser = Parser Char

readSexp :: String -> Sexp
readSexp input = case eater2trans (allowSpaceAfter parseSexp) input of
                   Error err -> E.throw $ ReaderException err
                   Success sexp -> sexp

readSexps :: String -> [Sexp]
readSexps input = case eater2trans (allowSpaceAfter (eatMany parseSexp)) input of
                   Error err -> E.throw $ ReaderException err
                   Success sexps -> sexps

space :: CharEater Char
space = oneOf " \t\n"

spaces :: CharEater ()
spaces = skipMany1 space

maybeSpaces :: CharEater ()
maybeSpaces = skipMany space

parseSimpleSymbol :: CharEater String
parseSimpleSymbol = eatMany1 (noneOf "()[] \n")

parseQuotedSymbol :: CharEater String
parseQuotedSymbol =
    do stream "["
       syms <- eatMany (eatOrs eaters)
       stream "]"                       
       return $ concat syms
    where eaters = [
           stream "\\[", 
           stream "\\]",
           (do stream "\\\\"
               return "\\"),
           liftM (\x -> [x]) (noneOf "[]"),
           (do s <- parseQuotedSymbol
               return $ "[" ++ s ++ "]")]

parseSymbol :: CharEater String
parseSymbol = do maybeSpaces
                 eatOr parseQuotedSymbol parseSimpleSymbol

parseNode :: CharEater Sexp
parseNode = do maybeSpaces 
               token '('
               maybeSpaces
               head <- parseSymbol
               children <- eatMany parseSexp
               maybeSpaces
               token ')'
               return (Node head children)      

parseSexp :: CharEater Sexp
parseSexp = eatOr (liftM Symbol parseSymbol) parseNode

allowSpaceAfter p = do res <- p
                       maybeSpaces
                       return res