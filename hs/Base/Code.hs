{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}
module Code (Code, layout,
             text, newline, indent, append, group,
             conc, joinBy, lines, paragraphs,
             space, words, embrace, parens, brackets,
             braces, commaSep) where
import Prelude hiding (catch, lines, words)
import Util
import Arrows
import Model

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

layoutSimple :: SimpleCode -> String
layoutSimple x =
    case x of
      Noc     -> ""
      Txt s x -> s ++ layoutSimple x
      Ind i x -> "\n" ++ copy i ' ' ++ layoutSimple x

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

--- Layout function ---

layout :: Int -> Code -> String
layout lineWidth code = layoutSimple (simp lineWidth 0 [(0,code)])

instance ToString Code where
    toString = toIO (layout 70)

--- Interfacing constructor functions ---

text s     = Text s
newline    = Newline
indent i x = Indent i x
append x y = Append x y
group x    = Or (flatten x) x

--- Utilities ---

indent2 = indent 2

conc :: [Code] -> Code
conc = foldr append NoCode

joinBy :: Code -> [Code] -> Code
joinBy c = conc . interleave c

lines :: [Code] -> Code
lines = joinBy newline

paragraphs :: [Code] -> Code
paragraphs = joinBy $ text "\n\n"

space :: Code
space = text " "

words :: [Code] -> Code
words = joinBy space

embrace :: String -> String -> Code -> Code
embrace l r code = conc [text l, code, text r]

parens :: Code -> Code
parens = embrace "(" ")"

brackets :: Code -> Code
brackets = embrace "[" "]"

braces :: Code -> Code
braces = embrace "{" "}"

commaSep :: [Code] -> Code
commaSep cs = joinBy (Text ", ") cs

tuple    = parens   . commaSep
list     = brackets . commaSep
wordList = parens   . words

binOp op x y = words x (text op) y
binParenOp op x y = parens $ binOp op x y
foldOp :: String -> [Code] -> Code
foldOp op = foldr1 (binOp op)
parenFoldOp op = parens . foldOp op


