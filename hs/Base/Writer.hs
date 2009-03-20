module Writer () where
import Prelude hiding (catch, lines, words)
import Util
import Sexp
import Code2String

instance Show Sexp where
    show sexp = "\n" ++ layoutSexp 25 sexp ++ "\n"

layoutSexp :: Int -> Sexp -> String
layoutSexp lineWidth = layout lineWidth . sexp2code

sexp2code :: Sexp -> Code
sexp2code sexp =
    case sexp of
      Sexp "space" []                      -> space
      Sexp "newline" []                    -> newline
      Sexp "text" [Sexp s []]              -> text s
      Sexp "indent" [Sexp i [], c]         -> indent (read i) (sexp2code c)
      Sexp "append" cs                     -> foldl1 append $ map sexp2code cs
      Sexp "group" [c]                     -> group (sexp2code c)
      Sexp "lines" cs                      -> lines (map sexp2code cs)
      Sexp "paragraphs" cs                 -> paragraphs (map sexp2code cs)
      Sexp "words" cs                      -> words (map sexp2code cs)
      Sexp "commaSep" cs                   -> commaSep (map sexp2code cs)
      Sexp "parens" [c]                    -> parens (sexp2code c)
      Sexp "brackets" [c]                  -> brackets (sexp2code c)
      Sexp "curlyBraces" [c]               -> curlyBraces (sexp2code c)
      Sexp "foldOp" (Sexp op [] : cs)      -> joinBy (Text (" " ++ op ++ " ")) (map sexp2code cs)
      Sexp "noFlat" [Sexp str stream]      -> nfPr (Text str : map sexp2code stream)
      Sexp str []                          -> Text str
      Sexp str stream                      -> prin (Text str : map sexp2code stream)
  where prin = parens . group . (indent 2) . lines
        nfPr = parens . (indent 2) . lines

