{-# OPTIONS -fglasgow-exts #-}
module Haskell2Code where
import Prelude hiding (lines, words)
import Control.Arrow
import Util
import Arrows
import Model
import Parser
import Code
import Haskell

instance Compilable (Haskell -> Code) Haskell Code where
    comp (Haskell mayMod tops) = paragraphs $ compMod mayMod ++ map comp tops
        where compMod Nothing  = []
              compMod (Just m) = [comp m]

instance Compilable (Module -> Code) Module Code where
    comp (Module name exports imports) = 
        (lines $
         [words $ [text "module", text name] ++ compExports exports ++ [text "where"]]
         ++ map comp imports)
        where compExports Nothing     = []
              compExports (Just (Export funs)) = [tuple $ map text funs]

instance Compilable (Import -> Code) Import Code where
    comp (Import modName impArgs) = words $ [text "import"] ++ bef ++ [text modName] ++ aft
        where (bef, aft) = case impArgs of
                             Simple -> ([], [])
                             Qualified abb -> ([text "qualified"], [text "as", text abb])
                             Only onlys    -> ([], [tuple (map text onlys)])
                             Hiding hides  -> ([], [text "hiding", tuple (map text hides)])

instance Compilable (Toplevel -> Code) Toplevel Code where
    comp (TopHasType ht)  = comp ht
    comp (TopDef d)       = comp d
    comp (TopTypeAlias t) = comp t
    comp (TopData d)      = comp d
    comp (TopClass c)     = comp c
    comp (TopInstance i)  = comp i

instance Compilable (HasType -> Code) HasType Code where
    comp (HasType expr mayDep typ) = binOp "::" (comp expr) (append (layoutMaybe comp mayDep) (comp typ))

instance Compilable (Type -> Code) Type Code where
    comp (NormalType str)   = text str
    comp (ListType t)       = brackets $ comp t
    comp (TupleType ts)     = tuple $ map comp ts
    comp (FunType ts)       = parenFoldOp "->" $ map comp ts
    comp (ParamType str ts) = wordList (text str : map comp ts)

withWhere :: Code -> Where -> Code
withWhere c wh = indent2 $ lines $ c : [comp wh]

instance Compilable (Def -> Code) Def Code where
    comp (Def pat expr mayWhere) =
        case mayWhere of 
          Nothing -> defi
          Just wh -> withWhere defi wh
      where defi = binOp "=" (comp pat) (comp expr)

instance Compilable (Expr -> Code) Expr Code where
    comp (LambdaExpr patterns expr) = 
        parens $ words $ [text "\\"] ++ map compPatternWithParens patterns ++ [text "->", comp expr]
    comp (DoExpr doCmds) = parens $ indent2 $ lines $ (text "do" : map comp doCmds)
    comp (TypeExpr typeDef) = comp typeDef
    comp (PatternExpr pattern) = compPatternWithParens pattern
                                            
instance Compilable (DoCmd -> Code) DoCmd Code where
    comp (DoAssign pat expr) = binOp "<-" (comp pat) (comp expr)
    comp (DoCmdExpr expr) = comp expr

instance Compilable (Where -> Code) Where Code where
    comp (Where tops) = indent2 $ lines $ text "where" : map comp tops

instance Compilable (Pattern -> Code) Pattern Code where
    comp (ListPattern pats)  = list $ map comp pats
    comp (TuplePattern pats) = tuple $ map comp pats
    comp (ConsPattern pats) = parenFoldOp ":" $ map comp pats
    comp (StringPattern str) = string str
    comp (CallPattern call)  = comp call

compPatternWithParens pattern =
    case pattern of
      CallPattern (FunCall _)      -> parens $ comp pattern
      CallPattern (OpFoldCall _ _) -> parens $ comp pattern
      _                            -> comp pattern

instance Compilable (Call -> Code) Call Code where
    comp (ConstCall str)        = text str
    comp (ConstOpCall str)      = parens $ text str
    comp (FunCall exprs)        = words (map comp exprs)
    comp (OpFoldCall str exprs) = foldOp str (map comp exprs)

instance Compilable (TypeAlias -> Code) TypeAlias Code where
    comp (TypeAlias new old) =
        words [text "type", comp new, text "=", comp old] 

instance Compilable (Data -> Code) Data Code where
    comp (Data typ constrs) =
        indent2 $ lines [words [text "data", comp typ, text "="], 
                         joinBy (append (text " |") newline) (map comp constrs)]

instance Compilable (Constructor -> Code) Constructor Code where
    comp (Constructor name types) = words $ text name : map comp types

instance Compilable (Class -> Code) Class Code where
    comp (Class mayDep typ whereClause) = 
        withWhere (words [text "class", append (layoutMaybe comp mayDep) (comp typ)]) whereClause                  

instance Compilable (Instance -> Code) Instance Code where
    comp (Instance mayDep typ whereClause) = 
        withWhere (words [text "instance", append (layoutMaybe comp mayDep) (comp typ)]) whereClause

instance Compilable (TypeDependancy -> Code) TypeDependancy Code where
    comp (TypeDependancy deps) = words [tuple (map comp deps), text "=> "]