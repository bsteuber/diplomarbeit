module Code2String (code2string) where
import Prelude hiding (catch, lines, words)
import Control.Exception
import Util
import Sexp
import Reader

defaultLineWidth = 60

code2string :: Sexp -> String
code2string = layoutSexp defaultLineWidth

instance Show Sexp where
    show sexp = "\n" ++ layoutSexp 25 sexp ++ "\n"

layoutSexp :: Int -> Sexp -> String
layoutSexp lineWidth = pprint lineWidth . sexp2code

sexp2code :: Sexp -> Code
sexp2code sexp =
    case sexp of
      Symbol "space"                       -> space
      Symbol "newline"                     -> newline
      Node "text" [Symbol s]               -> text s
      Node "indent" [Symbol i, c]          -> indent (read i) (sexp2code c)
      Node "append" cs                     -> foldl1 append $ map sexp2code cs
      Node "group" [c]                     -> group (sexp2code c)
      Node "lines" cs                      -> lines (map sexp2code cs)
      Node "paragraphs" cs                 -> paragraphs (map sexp2code cs)
      Node "words" cs                      -> words (map sexp2code cs)
      Node "commaSep" cs                   -> commaSep (map sexp2code cs)
      Node "parens" [c]                    -> parens (sexp2code c)
      Node "brackets" [c]                  -> brackets (sexp2code c)
      Node "curlyBraces" [c]               -> curlyBraces (sexp2code c)
      Node "foldOp" (Symbol op : cs)       -> joinBy (Text (" " ++ op ++ " ")) (map sexp2code cs)
      Node "noFlat" [Node str stream]      -> nfPr (Text str : map sexp2code stream)
      Node str stream                      -> prin (Text str : map sexp2code stream)
      Symbol str                           -> Text str
  where prin = parens . group . (indent 2) . lines
        nfPr = parens . (indent 2) . lines


--- Data Types ---

data Code = NoCode
          | Text String 
          | Newline
          | Indent Int Code
          | Append Code Code
          | Or Code Code

data SimpleCode = Noc
                | Txt String SimpleCode
                | Ind Int SimpleCode

--- Interfacing constructor functions ---

noCode     = NoCode
text s     = Text s
newline    = Newline
indent i x = Indent i x
append x y = Append x y
group x    = Or (flatten x) x

--- Implementation ---

flatten :: Code -> Code
flatten code =
    case code of
      NoCode       -> NoCode
      Text s       -> Text s
      Newline      -> Text " "
      Indent _ x   -> flatten x -- other than in book...
      Append x y   -> Append (flatten x) (flatten y)
      Or x _       -> flatten x

layout :: SimpleCode -> String
layout x = 
    case x of
      Noc     -> ""
      Txt s x -> s ++ layout x
      Ind i x -> "\n" ++ copy i ' ' ++ layout x

fits w x | w < 0 = False
fits w (Txt s x) = fits (w - length s) x 
fits _ _         = True

better w k x y = if fits (w - k) x then x else y

simp w k ts =
    case ts of
      []        -> Noc
      (i,c) : z ->
          case c of
            NoCode           -> simp w k z
            Text s           -> Txt s (simp w (k + length s) z)
            Newline          -> Ind i (simp w i z)
            Indent j x       -> simp w k ((i+j,x) : z)
            Append x y       -> simp w k ((i,x):(i,y):z)
            Or x y           -> 
                better w k (simp w k ((i,x):z)) (simp w k ((i,y):z))

pprint :: Int -> Code -> String
pprint lineWidth code = layout (simp lineWidth 0 [(0,code)])

conc :: [Code] -> Code
conc = foldr append noCode

joinBy :: Code -> [Code] -> Code
joinBy c = conc . interleave c

lines :: [Code] -> Code
lines = joinBy newline

paragraphs :: [Code] -> Code
paragraphs = joinBy $ text "\n\n"

space :: Code
space = Text " "

words :: [Code] -> Code
words = joinBy space

embrace :: String -> String -> Code -> Code
embrace l r code = conc [text l, code, text r]

parens :: Code -> Code
parens = embrace "(" ")"

brackets :: Code -> Code
brackets = embrace "[" "]"

curlyBraces :: Code -> Code
curlyBraces = embrace "{" "}"

commaSep :: [Code] -> Code
commaSep cs = joinBy (Text ", ") cs