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
    comp (TopTypeDef td) = comp td
    comp (TopDef d)      = comp d

instance Compilable (TypeDef -> Code) TypeDef Code where
    comp (TypeDef expr typ) = binOp "::" (comp expr) (comp typ)

instance Compilable (Type -> Code) Type Code where
    comp (NormalType str)   = text str
    comp (ListType t)       = brackets $ comp t
    comp (TupleType ts)     = tuple $ map comp ts
    comp (FunType ts)       = parenFoldOp "->" $ map comp ts
    comp (ParamType str ts) = wordList (text str : map comp ts)

instance Compilable (Def -> Code) Def Code where
    comp (Def pat expr mayWhere) =
        case mayWhere of 
          Nothing -> defi
          Just wh -> indent2 $ lines $ defi : [comp wh]
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