{-# OPTIONS -fglasgow-exts #-}
module Sexp2Haskell where
import Prelude hiding (lines, words)
import Control.Arrow
import Util
import Arrows
import Sexp
import Model
import Parser
import Compiler
import Haskell

isOp :: String -> Bool
isOp = all (`elem` "!$%&/=?*+-.:<|>")

instance Compilable (Parser Sexp () Haskell) [Sexp] Haskell where
    comp = macro "haskell" (liftA2 Haskell comp comp)

instance Compilable (Parser Sexp () Module) [Sexp] Module where
    comp = macro "module" (liftA3 Module takeSymbol comp comp)

instance Compilable (Parser Sexp () Export) [Sexp] Export where
    comp = liftA1 Export (macro "export" (many takeSymbol))

instance Compilable (Parser Sexp () Import) [Sexp] Import where
    comp  = (takeSymbol >>^ \ n -> Import n Simple) <+>
             parseNode (liftA2 Import takeSymbol compArgs)
        where compArgs =
                  ((empty >>> constArrow Simple)               <+> 
                   (macro "qualified" takeSymbol >>> arr Qualified) <+>
                   (macro "only" (many takeSymbol) >>> arr Only)           <+>
                   (macro "hiding" (many takeSymbol) >>> arr Hiding))

instance Compilable (Parser Sexp () Toplevel) [Sexp] Toplevel where
    comp = (comp >>^ TopTypeDef) <+> (comp >>^ TopDef)

instance Compilable (Parser Sexp () TypeDef) [Sexp] TypeDef where
    comp = macro "type" (liftA2 TypeDef comp comp)

instance Compilable (Parser Sexp () Type) [Sexp] Type where
    comp = ((symbolMacro "Unit" >>> constArrow (TupleType [])) <+> 
             (liftA1  TupleType     (macro "Tuple" comp))      <+>
             (liftA1  ListType      (macro "List"  comp))      <+>
             (liftA1  FunType       (macro "Fun"   comp))      <+>
             (parseNode (liftA2 ParamType takeSymbol comp))         <+>
             (liftA1  NormalType    takeSymbol))

instance Compilable (Parser Sexp () Def) [Sexp] Def where
    comp = macro "=" $ liftA3 Def comp comp comp

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
         liftA1 StringPattern (macro "Str" (many takeSymbol >>> arr unwords))  <+>
         liftA1 CallPattern comp)

instance Compilable (Parser Sexp () Call) [Sexp] Call where
    comp = 
        (liftA1 ConstOpCall (takeSymbol >>> failUnless isOp)      <+>
         liftA1 ConstCall   (takeSymbol <+> parseNode takeSymbol) <+>
         parseNode (liftA2 
                    OpFoldCall 
                    (takeSymbol >>> failUnless isOp)
                    comp)                                         <+>
         parseNode (liftA1 FunCall comp))
