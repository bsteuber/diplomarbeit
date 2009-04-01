module Haskell2Code where
import Prelude hiding (lines, words)
import Control.Arrow
import Util
import Arrows
import Sexp
import Parser
import Compiler
import Code

data Haskell      = Haskell (Maybe Module) [Toplevel]
data Module       = Module ModuleName (Maybe Exports) [Import]
type ModuleName   = String
type Exports      = [String]
data Import       = Import ModuleName ImportArgs
data ImportArgs   = Simple
                  | Only [FunctionName]
                  | Hiding [FunctionName]
                  | Qualified ModuleAbbrev
type FunctionName = String
type ModuleAbbrev = String
data Toplevel     = TopTypeDef TypeDef
                  | TopDef Def
data TypeDef      = TypeDef Expr Type
data Type         = NormalType String
                  | ListType Type
                  | TupleType [Type]
                  | FunType [Type]
--data Expr         = 

parseHaskell   = macro "haskell" (liftA2 Haskell (optional parseModule) (many parseTopLevel))
parseModule    = macro "module"  (liftA3 Module takeSymbol (optional (many takeSymbol)) (many parseImport
parseImport    =
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

parseUnit = symbolMacro "Unit" (tuple [])
parseList parseInner = macro "List" (parseInner >>> arr list)
parseTuple parseInner = macro "Tuple" (parseInner >>> arr tuple)

parseTyped :: SexpParser a Sexp
parseTyped = (parseUnit <+>
             parseList (parseTyped >>> arr single) <+>
             parseTuple (many parseTyped) <+>
             symbol2text <+>
             (macro "Fun" (many parseTyped >>> arr (parenFoldOp "->"))) <+>
             (takeAnySexp ((arr snd &&& many parseTyped) >>>
                           arr (\ (name, innerTypes) -> wordList $ text name : innerTypes))))

isOp :: String -> Bool
isOp = all (`elem` "!$%&/=?*+-.:<|>")

genericParseCall :: (Sexp -> Sexp) -> SexpParser a Sexp
genericParseCall processCall =
    (takeSymbol >>> arr (\ s -> if isOp s then tuple [text s] else text s)) <+>
    (takeSexp >>> (arr fst &&& runParser (many parseExpr)) >>> arr (processCall . buildCall))
        where buildCall (name, innerExprs) =
                  if isOp name then
                      foldOp name innerExprs
                  else
                      words $ text name : innerExprs

parseCall = genericParseCall parens

genericParsePattern processCall =
    (parseUnit <+>
     parseList (many parsePattern) <+>
     parseTuple (many parsePattern) <+>
     parseString <+>
     genericParseCall processCall)

parsePattern = genericParsePattern parens
parsePatternWithoutParens :: SexpParser a Sexp
parsePatternWithoutParens = genericParsePattern id

parseType :: SexpParser a Sexp
parseType = macro "type" (liftA2 (binOp "::") parseExpr parseTyped)

parseString = macro "str" (many symbol2text >>> arr makeStr)
    where makeStr exps = append $ strQ ++ exps ++ strQ
          strQ = [text "\""]


parseLambda = macro "fun" ((parseArgs &&& parseExpr) >>> arr buildLambda)
    where parseArgs = macro "args" (many parsePattern) <+> (parsePattern >>> arr single)
          buildLambda (args, expr) = wordList $  [text "\\"] ++ args ++ [text "->", expr]

parseDo = macro "do" ((many (macro "<-" (liftA2
                                        (binOp "<-")
                                        parsePattern parseExpr)
                            <+> parseExpr))
                     >>> arr buildDo)
    where buildDo = parens . indent . lines . (text "do":)

parseExpr :: SexpParser a Sexp
parseExpr = (parseLambda <+>
            parseDo <+>
            parseType <+>
            parsePattern)

parseDef :: SexpParser a Sexp
parseDef = macro "=" ((liftA2 (binOp "=") parsePatternWithoutParens parseExpr &&& parseWhere) >>> arr buildDef)
    where parseWhere = optional (macro "where" (many parseToplevel)) >>> arr concat
          buildDef :: (Sexp, [Sexp]) -> Sexp
          buildDef (def, []) = def
          buildDef (def, whereDefs) =
              append [def, indent (append [newline, indent $ lines $ text "where" : whereDefs])]

parseToplevel = parseType <+> parseDef

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
    where compWhere = optional (macro "where" (many compToplevel)) >>> arr concat
          buildDef :: (Sexp, [Sexp]) -> Sexp
          buildDef (def, []) = def
          buildDef (def, whereDefs) =
              append [def, indent (append [newline, indent $ lines $ text "where" : whereDefs])]

compToplevel = compType <+> compDef

haskell2code = macro "haskell" (liftA2
                                (\a b -> paragraphs (a ++ b))
                                (optional compModule)
                                (many compToplevel))
