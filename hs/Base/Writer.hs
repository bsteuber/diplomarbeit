module Writer () where
import Prelude hiding (catch, lines, words)
import Util
import Sexp
import Code

instance OfSexp Code where
    ofSexp (Sexp str [])     = text str
    ofSexp (Sexp str stream) = prin (text str : map ofSexp stream)
        where prin = parens . group . (indent 2) . lines

instance ToString Sexp where
    toString sexp = toString code
        where code :: Code
              code = ofSexp sexp

-- scode2code :: Sexp -> Code
-- scode2code sexp =
--     case sexp of
--       Sexp "space" []                      -> space
--       Sexp "newline" []                    -> newline
--       Sexp "text" [Sexp s []]              -> text s
--       Sexp "indent" [Sexp i [], c]         -> indent (read i) (scode2code c)
--       Sexp "append" cs                     -> foldl1 append $ map scode2code cs
--       Sexp "group" [c]                     -> group (scode2code c)
--       Sexp "lines" cs                      -> lines (map scode2code cs)
--       Sexp "paragraphs" cs                 -> paragraphs (map scode2code cs)
--       Sexp "words" cs                      -> words (map scode2code cs)
--       Sexp "commaSep" cs                   -> commaSep (map scode2code cs)
--       Sexp "parens" [c]                    -> parens (scode2code c)
--       Sexp "brackets" [c]                  -> brackets (scode2code c)
--       Sexp "braces" [c]                    -> curlyBraces (scode2code c)
--       Sexp "foldOp" (Sexp op [] : cs)      -> joinBy (Text (" " ++ op ++ " ")) (map scode2code cs)
--       other                                -> error $ "No Code Sexp:\n" ++ show other



