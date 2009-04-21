{-# OPTIONS -fglasgow-exts #-}
module Sexp2Haskell where
import Prelude hiding (lines, words)
import Control.Arrow
import Util
import Arrows
import Model
import Parser
import Sexp
import Haskell

isOp :: String -> Bool
isOp = all (`elem` "!$%&/=?*+-.:<|>")

instance Compilable (ExecFunParser Sexp Haskell) [Sexp] Haskell where
    comp = macro "haskell" (liftA2 Haskell comp comp)

instance Compilable (ExecFunParser Sexp Module) [Sexp] Module where
    comp = macro "module" (liftA3 Module takeSymbol comp comp)

instance Compilable (ExecFunParser Sexp Export) [Sexp] Export where
    comp = liftA1 Export (macro "export" (many takeSymbol))

instance Compilable (ExecFunParser Sexp Import) [Sexp] Import where
    comp  = (takeSymbol >>^ \ n -> Import n Simple) <+>
             compNode (liftA2 Import takeSymbol compArgs)
        where compArgs =
                  ((empty >>> constArrow Simple)               <+> 
                   (macro "qualified" takeSymbol >>> arr Qualified) <+>
                   (macro "only" (many takeSymbol) >>> arr Only)           <+>
                   (macro "hiding" (many takeSymbol) >>> arr Hiding))

instance Compilable (ExecFunParser Sexp Toplevel) [Sexp] Toplevel where
    comp = (comp >>^ TopTypeDef) <+> (comp >>^ TopDef)

instance Compilable (ExecFunParser Sexp TypeDef) [Sexp] TypeDef where
    comp = macro "type" (liftA2 TypeDef comp comp)

instance Compilable (ExecFunParser Sexp Type) [Sexp] Type where
    comp = ((symbolMacro "Unit" >>> constArrow (TupleType [])) <+> 
             (liftA1  TupleType     (macro "Tuple" comp))      <+>
             (liftA1  ListType      (macro "List"  comp))      <+>
             (liftA1  FunType       (macro "Fun"   comp))      <+>
             (compNode (liftA2 ParamType takeSymbol comp))         <+>
             (liftA1  NormalType    takeSymbol))

instance Compilable (ExecFunParser Sexp Def) [Sexp] Def where
    comp = macro "=" $ liftA3 Def comp comp comp

instance Compilable (ExecFunParser Sexp Expr) [Sexp] Expr where
    comp = 
        (macro "fun" (liftA2 LambdaExpr (macro "args" comp <+> (comp >>^ single)) comp) <+>
         liftA1 DoExpr (macro "do" comp)                                                  <+>
         liftA1 TypeExpr comp                                                             <+>
         liftA1 PatternExpr comp)

instance Compilable (ExecFunParser Sexp DoCmd) [Sexp] DoCmd where
    comp =
        (macro "<-" (liftA2 DoAssign comp comp) <+>
         liftA1 DoCmdExpr comp)

instance Compilable (ExecFunParser Sexp Where) [Sexp] Where where
    comp = macro "where" (liftA1 Where comp)

instance Compilable (ExecFunParser Sexp Pattern) [Sexp] Pattern where
    comp = 
        (liftA1 ListPattern (macro "List" comp)   <+>
         liftA1 TuplePattern (macro "Tuple" comp) <+>
         liftA1 StringPattern (macro "Str" (many takeSymbol >>> arr unwords))  <+>
         liftA1 CallPattern comp)

instance Compilable (ExecFunParser Sexp Call) [Sexp] Call where
    comp = 
        (liftA1 ConstOpCall (takeSymbol >>> failUnless isOp)      <+>
         liftA1 ConstCall   (takeSymbol <+> compNode takeSymbol) <+>
         compNode (liftA2 
                    OpFoldCall 
                    (takeSymbol >>> failUnless isOp)
                    comp)                                         <+>
         compNode (liftA1 FunCall comp))
