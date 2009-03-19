module Printer (compileStr, testMacro) where
import Sexp
import Code2String

compileStr :: Macro -> String -> String
compileStr macro = code2string . 
                   trans2fun CompileException macro . 
                   readSexp

testMacro :: Macro -> [(String, String)] -> IO ()
testMacro _ [] = do putStrLn "Hooray! Tests passed."
                    return ()
testMacro macro ((src, tgt):cases) = 
    (E.catch 
     (let res = compileStr macro src
          exp = (code2string $ readSexp tgt)
      in 
        if res == exp
        then
            testMacro macro cases
        else
            ("Test " ++ (show $ readSexp src) ++ ":\n  Expected:\n" ++ exp ++ "\n  Got:\n" ++ res) |> TestException |> E.throw)
     catchFun)
    where catchFun :: MagiclException -> IO ()
          catchFun e = ("Error when compiling test case:\n" ++ show src ++ "\n" ++ show e) |> putStrLn

instance Show Sexp where
    show sexp = "\n" ++ layoutSexp 25 sexp ++ "\n"

layoutSexp :: Int -> Sexp -> String
layoutSexp lineWidth = layout lineWidth . sexp2code

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

