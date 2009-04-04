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
type FunctionName = String
type ModuleAbbrev = String

data Haskell = Haskell (Maybe Module) [Toplevel]

data Module = Module ModuleName (Maybe Export) [Import]

data Export = Export [String]

data ImportArgs = Simple
                | Qualified ModuleAbbrev
                | Only [FunctionName]
                | Hiding [FunctionName]

data Import = Import ModuleName ImportArgs

data Toplevel = TopTypeDef TypeDef
              | TopDef Def

data TypeDef = TypeDef Expr Type

data Type = NormalType String
          | ListType Type
          | TupleType [Type]
          | FunType [Type]
          | ParamType String [Type]

data Def = Def Pattern Expr (Maybe Where)

data Expr = LambdaExpr  [Pattern] Expr
          | DoExpr      [DoCmd]
          | TypeExpr    Type
          | PatternExpr Pattern

data DoCmd = DoAssign Pattern Expr
           | DoCmdExpr Expr

data Where = Where [Toplevel]

data Pattern = ListPattern [Pattern]
             | TuplePattern [Pattern]
             | StringPattern String
             | CallPattern Call

data Call = ConstCall String
          | ConstOpCall String
          | FunCall Expr [Expr]
          | OpFoldCall String [Expr]

isOp :: String -> Bool
isOp = all (`elem` "!$%&/=?*+-.:<|>")

instance Compilable (Parser Sexp () Haskell) [Sexp] Haskell where
    comp = macro "haskell" (liftA2 Haskell comp comp)

instance Compilable (Parser Sexp () Module) [Sexp] Module where
    comp = macro "module" (liftA3 Module comp comp comp)

instance Compilable (Parser Sexp () Export) [Sexp] Export where
    comp = liftA1 Export (macro "export" comp)

instance Compilable (Parser Sexp () Import) [Sexp] Import where
    comp  = (takeSymbol >>^ \ n -> Import n Simple) <+>
             parseNode (liftA2 Import comp compArgs)
        where compArgs =
                  ((empty >>> constArrow Simple)               <+> 
                   (macro "qualified" comp >>> arr Qualified) <+>
                   (macro "only" comp >>> arr Only)           <+>
                   (macro "hiding" comp >>> arr Hiding))

instance Compilable (Parser Sexp () Toplevel) [Sexp] Toplevel where
    comp = (comp >>^ TopTypeDef) <+> (comp >>^ TopDef)

instance Compilable (Parser Sexp () TypeDef) [Sexp] TypeDef where
    comp = liftA2 TypeDef comp comp

instance Compilable (Parser Sexp () Type) [Sexp] Type where
    comp = ((symbolMacro "Unit" >>> constArrow (TupleType [])) <+> 
             (liftA1  TupleType     (macro "Tuple" comp))      <+>
             (liftA1  ListType      (macro "List"  comp))      <+>
             (liftA1  FunType       (macro "Fun"   comp))      <+>
             (parseNode (liftA2 ParamType comp comp))         <+>
             (liftA1  NormalType    comp))

instance Compilable (Parser Sexp () Def) [Sexp] Def where
    comp = liftA3 Def comp comp comp

instance Compilable (Parser Sexp () Expr) [Sexp] Expr where
    comp = 
        (macro "fun" (liftA2 LambdaExpr (macro "args" comp <+> (comp >>^ single)) comp) <+>
         liftA1 DoExpr (macro "do" comp)                                                  <+>
         liftA1 TypeExpr comp                                                             <+>
         liftA1 PatternExpr comp)

instance Compilable (Parser Sexp () DoCmd) [Sexp] DoCmd where
    comp =
        (macro "<-" (liftA2 DoAssign comp comp) <+>
         liftA1 DoCmdExpr comp)

instance Compilable (Parser Sexp () Where) [Sexp] Where where
    comp = macro "where" (liftA1 Where comp)

instance Compilable (Parser Sexp () Pattern) [Sexp] Pattern where
    comp = 
        (liftA1 ListPattern (macro "List" comp)   <+>
         liftA1 TuplePattern (macro "Tuple" comp) <+>
         liftA1 StringPattern (macro "Str" comp)  <+>
         liftA1 CallPattern comp)

instance Compilable (Parser Sexp () Call) [Sexp] Call where
    comp = 
        (liftA1 ConstOpCall (comp >>> failUnless isOp) <+>
         liftA1 ConstCall   comp                       <+>
         parseNode (liftA2 
                    OpFoldCall 
                    (comp >>> failUnless isOp)
                    comp)                              <+>
         parseNode (liftA2 FunCall comp comp))

instance FunComp Module Code where
    comp (Module name exports imports) = 
        (lines $
         [words $ [text "module", text name] ++ compExports exports ++ [text "where"]]
         ++ map comp imports)
        where compExports Nothing     = []
              compExports (Just (Export funs)) = [tuple $ map text funs]

instance FunComp Import Code where
    comp (Import modName impArgs) = words $ [text "import"] ++ bef ++ [text modName] ++ aft
        where (bef, aft) = case impArgs of
                             Simple -> ([], [])
                             Qualified abb -> ([text "qualified"], [text "as", text abb])
                             Only onlys    -> ([], [tuple (map text onlys)])
                             Hiding hides  -> ([], [text "hiding", tuple (map text hides)])


instance FunComp TypeDef Code where
    comp (TypeDef expr typ) = binOp "::" (comp expr) (comp typ)

instance FunComp Type Code where
    comp (NormalType str)   = text str
    comp (ListType t)       = brackets $ comp t
    comp (TupleType ts)     = tuple $ map comp ts
    comp (FunType ts)       = parenFoldOp "->" $ map comp ts
    comp (ParamType str ts) = wordList (text str : map comp ts)

instance Compilable Module Code where
    compile = toIO comp                                                

instance Compilable Import Code where
    compile = toIO comp

instance Compilable TypeDef Code where
    compile = toIO comp

instance Compilable Type Code where
    compile = toIO comp



-- parseExpr :: SexpParser a Sexp
-- parseExpr = (parseLambda <+>
--             parseDo <+>
--             parseType <+>
--             parsePattern)


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
