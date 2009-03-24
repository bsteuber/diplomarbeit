module Haskell2Code where
import Prelude hiding (lines, words)
import Control.Arrow
import Util
import Arrows
import Sexp
import Parser
import Compiler

text :: String -> Sexp
text = singleNode "text" . symbol

newline :: Sexp
newline = symbol "newline"

parens :: Sexp -> Sexp
parens = singleNode "parens"

brackets :: Sexp -> Sexp
brackets = singleNode "brackets"

lines = node "lines"
append = node "append"
paragraphs = node "paragraphs"
words = node "words"
textWords = map text >>> words
commaSep = node "commaSep"
noFlat = singleNode "noFlat"
indent x = node "indent" [symbol "2", x]

foldOp :: String -> [Sexp] -> Sexp
foldOp op stream = node "foldOp" (symbol op : stream)

parenFoldOp op = parens . foldOp op

binOp op x y = foldOp op [x, y]
binParenOp op x y = parens $ binOp op x y

tuple    = parens   . commaSep
list     = brackets . commaSep
wordList = parens   . words

symbol2text = takeSymbol >>> arr text

compModule :: SexpParser a Sexp
compModule = 
    macro "module" (consArrow
                    (takeSymbol >>> arr (\s -> textWords ["module", s, "where"]))
                    (many compImport)
                   ) >>> arr lines

compImport :: SexpParser a Sexp
compImport = 
    takeAnySexp $ (arr snd &&&
                 (impNormal <+> impQualified <+> impHiding <+> impOnly)) >>> arr imp
    where impNormal              = empty >>> constArrow ([], [])
          impQualified           = macro "qualified" (takeSymbol >>> arr calcQualified)
          calcQualified short    = ([text "qualified"], [text "as", text short])
          impHiding              = macro "hiding" (many takeSymbol >>> arr calcHiding)
          calcHiding hides       = ([], [text "hiding", tuple (map text hides)])
          impOnly                = macro "only" (many takeSymbol >>> arr calcOnly)
          calcOnly onlys         = ([], [tuple (map text onlys)])
          imp (name, (bef, aft)) = words $ [text "import"] ++ bef ++ [text name] ++ aft

compUnit = symbolMacro "Unit" (tuple [])
compList compInner = macro "List" (compInner >>> arr list)
compTuple compInner = macro "Tuple" (compInner >>> arr tuple)

compTyped :: SexpParser a Sexp
compTyped = (compUnit <+>
             compList (compTyped >>> arr single) <+>
             compTuple (many compTyped) <+>
             symbol2text <+>
             (macro "Fun" (many compTyped >>> arr (parenFoldOp "->"))) <+>
             (takeAnySexp ((arr snd &&& many compTyped) >>> 
                           arr (\ (name, innerTypes) -> wordList $ text name : innerTypes))))

isOp :: String -> Bool
isOp = all (`elem` "!$%&/=?*+-.:<|>")

genericCompCall :: (Sexp -> Sexp) -> SexpParser a Sexp
genericCompCall processCall = 
    (takeSymbol >>> arr (\ s -> if isOp s then tuple [text s] else text s)) <+>
    (takeSexp >>> (arr fst &&& runParser (many compExpr)) >>> arr (processCall . buildCall))
        where buildCall (name, innerExprs) =
                  if isOp name then 
                      foldOp name innerExprs
                  else
                      words $ text name : innerExprs

compCall = genericCompCall parens

genericCompPattern processCall =
    (compUnit <+>
     compList (many compPattern) <+>
     compTuple (many compPattern) <+>
     compString <+>
     genericCompCall processCall)

compPattern = genericCompPattern parens
compPatternWithoutParens :: SexpParser a Sexp
compPatternWithoutParens = genericCompPattern id

compType :: SexpParser a Sexp
compType = macro "type" (liftA2 (binOp "::") compExpr compTyped)

compString = macro "str" (many symbol2text >>> arr makeStr)
    where makeStr exps = append $ strQ ++ exps ++ strQ
          strQ = [text "\""]


compLambda = macro "fun" ((compArgs &&& compExpr) >>> arr buildLambda)
    where compArgs = macro "args" (many compPattern) <+> (compPattern >>> arr single)
          buildLambda (args, expr) = wordList $  [text "\\"] ++ args ++ [text "->", expr]

compDo = macro "do" ((many (macro "<-" (liftA2 
                                        (binOp "<-")
                                        compPattern compExpr)
                            <+> compExpr))
                     >>> arr buildDo)
    where buildDo = parens . indent . lines . (text "do":)                            

compExpr :: SexpParser a Sexp
compExpr = (compLambda <+>
            compDo <+>
            compType <+>
            compPattern)                 

compDef :: SexpParser a Sexp
compDef = macro "=" ((liftA2 (binOp "=") compPatternWithoutParens compExpr &&& compWhere) >>> arr buildDef)
    where compWhere = optional (macro "where" (many compTopLevel)) >>> arr concat
          buildDef :: (Sexp, [Sexp]) -> Sexp
          buildDef (def, []) = def
          buildDef (def, whereDefs) =                                  
              append [def, indent (append [newline, indent $ lines $ text "where" : whereDefs])]

compTopLevel = compType <+> compDef

haskell2code = macro "haskell" (liftA2 
                                (\a b -> paragraphs (a ++ b)) 
                                (optional compModule)
                                (many compTopLevel))
          