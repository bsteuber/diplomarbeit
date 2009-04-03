{-# OPTIONS -fglasgow-exts #-}
module Haskell2Code where
import Prelude hiding (lines, words)
import Control.Arrow
import Util
import Arrows
import Sexp
import Model
import Parser
import Compiler
import Code

type ModuleName = String
type Exports = [String]
type FunctionName = String
type ModuleAbbrev = String


data Haskell = Haskell (Maybe Module) [Toplevel]
instance Parsable Sexp Haskell where
    parse = macro "haskell" (liftA2 Haskell parse parse)

data Module = Module ModuleName (Maybe Exports) [Import]
instance Parsable Sexp Module where
    parse = macro "module" (liftA3 Module parse parse parse)

data Import = Import ModuleName ImportArgs
instance Parsable Sexp Import where
    parse  = parseNode (liftA2 Import parse parse)
instance Compilable Import Code where
    compile = toIO comp where
        comp (Import modName impArgs) = words $ [text "import"] ++ bef ++ [text modName] ++ aft
            where (bef, aft) = case impArgs of
                                 Simple -> ([], [])
                                 Qualified abb -> ([text "qualified"], [text "as", text abb])
                                 Only onlys    -> ([], [tuple (map text onlys)])
                                 Hiding hides  -> ([], [text "hiding", tuple (map text hides)])

data ImportArgs = Simple
                | Qualified ModuleAbbrev
                | Only [FunctionName]
                | Hiding [FunctionName]
instance Parsable Sexp ImportArgs where
    parse = (empty >>> constArrow Simple)               <+> 
            (macro "qualified" parse >>> arr Qualified) <+>
            (macro "only" parse >>> arr Only)           <+>
            (macro "hiding" parse >>> arr Hiding)

data Toplevel = TopTypeDef TypeDef
              | TopDef Def
instance Parsable Sexp Toplevel where
    parse = (parse >>^ TopTypeDef) <+> (parse >>^ TopDef)

data TypeDef = TypeDef Expr Type
instance Parsable Sexp TypeDef where
    parse = liftA2 TypeDef parse parse

data Type = NormalType String
          | ListType Type
          | TupleType [Type]
          | FunType [Type]
instance Parsable Sexp Type where
    parse = liftA1 NormalType parse

data Def = Def String
instance Parsable Sexp Def where
    parse = liftA1 Def parse
data Expr = Expr String
instance Parsable Sexp Expr where
    parse = liftA1 Expr parse




-- parseUnit = symbolMacro "Unit" (tuple [])
-- parseList parseInner = macro "List" (parseInner >>> arr list)
-- parseTuple parseInner = macro "Tuple" (parseInner >>> arr tuple)

-- parseTyped :: SexpParser a Sexp
-- parseTyped = (parseUnit <+>
--              parseList (parseTyped >>> arr single) <+>
--              parseTuple (many parseTyped) <+>
--              symbol2text <+>
--              (macro "Fun" (many parseTyped >>> arr (parenFoldOp "->"))) <+>
--              (takeAnySexp ((arr snd &&& many parseTyped) >>>
--                            arr (\ (name, innerTypes) -> wordList $ text name : innerTypes))))

-- isOp :: String -> Bool
-- isOp = all (`elem` "!$%&/=?*+-.:<|>")

-- genericParseCall :: (Sexp -> Sexp) -> SexpParser a Sexp
-- genericParseCall processCall =
--     (parse >>> arr (\ s -> if isOp s then tuple [text s] else text s)) <+>
--     (takeSexp >>> (arr fst &&& runParser (many parseExpr)) >>> arr (processCall . buildCall))
--         where buildCall (name, innerExprs) =
--                   if isOp name then
--                       foldOp name innerExprs
--                   else
--                       words $ text name : innerExprs

-- parseCall = genericParseCall parens

-- genericParsePattern processCall =
--     (parseUnit <+>
--      parseList (many parsePattern) <+>
--      parseTuple (many parsePattern) <+>
--      parseString <+>
--      genericParseCall processCall)

-- parsePattern = genericParsePattern parens
-- parsePatternWithoutParens :: SexpParser a Sexp
-- parsePatternWithoutParens = genericParsePattern id

-- parseType :: SexpParser a Sexp
-- parseType = macro "type" (liftA2 (binOp "::") parseExpr parseTyped)

-- parseString = macro "str" (many symbol2text >>> arr makeStr)
--     where makeStr exps = append $ strQ ++ exps ++ strQ
--           strQ = [text "\""]


-- parseLambda = macro "fun" ((parseArgs &&& parseExpr) >>> arr buildLambda)
--     where parseArgs = macro "args" (many parsePattern) <+> (parsePattern >>> arr single)
--           buildLambda (args, expr) = wordList $  [text "\\"] ++ args ++ [text "->", expr]

-- parseDo = macro "do" ((many (macro "<-" (liftA2
--                                         (binOp "<-")
--                                         parsePattern parseExpr)
--                             <+> parseExpr))
--                      >>> arr buildDo)
--     where buildDo = parens . indent . lines . (text "do":)

-- parseExpr :: SexpParser a Sexp
-- parseExpr = (parseLambda <+>
--             parseDo <+>
--             parseType <+>
--             parsePattern)

-- parseDef :: SexpParser a Sexp
-- parseDef = macro "=" ((liftA2 (binOp "=") parsePatternWithoutParens parseExpr &&& parseWhere) >>> arr buildDef)
--     where parseWhere = optional (macro "where" (many parseToplevel)) >>> arr concat
--           buildDef :: (Sexp, [Sexp]) -> Sexp
--           buildDef (def, []) = def
--           buildDef (def, whereDefs) =
--               append [def, indent (append [newline, indent $ lines $ text "where" : whereDefs])]

-- parseToplevel = parseType <+> parseDef

-- compModule :: SexpParser a Sexp
-- compModule =
--     macro "module" (consArrow
--                     (takeSymbol >>> arr (\s -> textWords ["module", s, "where"]))
--                     (many compImport)
--                    ) >>> arr lines

-- compImport :: SexpParser a Sexp
-- compImport =
--     takeAnySexp $ (arr snd &&&
--                  (impNormal <+> impQualified <+> impHiding <+> impOnly)) >>> arr imp

-- compUnit = symbolMacro "Unit" (tuple [])
-- compList compInner = macro "List" (compInner >>> arr list)
-- compTuple compInner = macro "Tuple" (compInner >>> arr tuple)

-- compTyped :: SexpParser a Sexp
-- compTyped = (compUnit <+>
--              compList (compTyped >>> arr single) <+>
--              compTuple (many compTyped) <+>
--              symbol2text <+>
--              (macro "Fun" (many compTyped >>> arr (parenFoldOp "->"))) <+>
--              (takeAnySexp ((arr snd &&& many compTyped) >>>
--                            arr (\ (name, innerTypes) -> wordList $ text name : innerTypes))))

-- isOp :: String -> Bool
-- isOp = all (`elem` "!$%&/=?*+-.:<|>")

-- genericCompCall :: (Sexp -> Sexp) -> SexpParser a Sexp
-- genericCompCall processCall =
--     (takeSymbol >>> arr (\ s -> if isOp s then tuple [text s] else text s)) <+>
--     (takeSexp >>> (arr fst &&& runParser (many compExpr)) >>> arr (processCall . buildCall))
--         where buildCall (name, innerExprs) =
--                   if isOp name then
--                       foldOp name innerExprs
--                   else
--                       words $ text name : innerExprs

-- compCall = genericCompCall parens

-- genericCompPattern processCall =
--     (compUnit <+>
--      compList (many compPattern) <+>
--      compTuple (many compPattern) <+>
--      compString <+>
--      genericCompCall processCall)

-- compPattern = genericCompPattern parens
-- compPatternWithoutParens :: SexpParser a Sexp
-- compPatternWithoutParens = genericCompPattern id

-- compType :: SexpParser a Sexp
-- compType = macro "type" (liftA2 (binOp "::") compExpr compTyped)

-- compString = macro "str" (many symbol2text >>> arr makeStr)
--     where makeStr exps = append $ strQ ++ exps ++ strQ
--           strQ = [text "\""]


-- compLambda = macro "fun" ((compArgs &&& compExpr) >>> arr buildLambda)
--     where compArgs = macro "args" (many compPattern) <+> (compPattern >>> arr single)
--           buildLambda (args, expr) = wordList $  [text "\\"] ++ args ++ [text "->", expr]

-- compDo = macro "do" ((many (macro "<-" (liftA2
--                                         (binOp "<-")
--                                         compPattern compExpr)
--                             <+> compExpr))
--                      >>> arr buildDo)
--     where buildDo = parens . indent . lines . (text "do":)

-- compExpr :: SexpParser a Sexp
-- compExpr = (compLambda <+>
--             compDo <+>
--             compType <+>
--             compPattern)

-- compDef :: SexpParser a Sexp
-- compDef = macro "=" ((liftA2 (binOp "=") compPatternWithoutParens compExpr &&& compWhere) >>> arr buildDef)
--     where compWhere = optional (macro "where" (many compToplevel)) >>> arr concat
--           buildDef :: (Sexp, [Sexp]) -> Sexp
--           buildDef (def, []) = def
--           buildDef (def, whereDefs) =
--               append [def, indent (append [newline, indent $ lines $ text "where" : whereDefs])]

-- compToplevel = compType <+> compDef

-- haskell2code = macro "haskell" (liftA2
--                                 (\a b -> paragraphs (a ++ b))
--                                 (optional compModule)
--                                 (many compToplevel))
