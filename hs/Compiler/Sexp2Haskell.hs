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

instance Compilable (SexpParser Haskell) [Sexp] Haskell where
    comp = macro "haskell" (liftA2 Haskell comp comp)

instance Compilable (SexpParser Module) [Sexp] Module where
    comp = macro "module" (liftA3 Module comp comp comp)

instance Compilable (SexpParser Export) [Sexp] Export where
    comp = liftA1 Export (macro "export" (many comp))

instance Compilable (SexpParser Import) [Sexp] Import where
    comp  = (comp >>^ \ n -> Import n Simple) <+>
             compNode (liftA2 Import comp compArgs)
        where compArgs =
                  ((empty >>> constArrow Simple)               <+> 
                   (macro "qualified" comp >>> arr Qualified) <+>
                   (macro "only" (many comp) >>> arr Only)           <+>
                   (macro "hiding" (many comp) >>> arr Hiding))

instance Compilable (SexpParser Toplevel) [Sexp] Toplevel where
    comp = (comp >>^ TopTypeDef) <+> (comp >>^ TopDef) <+> (comp >>^ TopInstance)

instance Compilable (SexpParser TypeDef) [Sexp] TypeDef where
    comp = macro "type" (liftA2 TypeDef comp comp)

instance Compilable (SexpParser Type) [Sexp] Type where
    comp = ((symbolMacro "Unit" >>> constArrow (TupleType [])) <+> 
             (liftA1  TupleType     (macro "Tuple" comp))      <+>
             (liftA1  ListType      (macro "List"  comp))      <+>
             (liftA1  FunType       (macro "Fun"   comp))      <+>
             (compNode (liftA2 ParamType comp comp))         <+>
             (liftA1  NormalType    comp))

instance Compilable (SexpParser Def) [Sexp] Def where
    comp = macro "=" $ liftA3 Def comp comp comp

instance Compilable (SexpParser Expr) [Sexp] Expr where
    comp = 
        (macro "fun" (liftA2 LambdaExpr (macro "args" comp <+> (comp >>^ single)) comp) <+>
         liftA1 DoExpr (macro "do" comp)                                                  <+>
         liftA1 TypeExpr comp                                                             <+>
         liftA1 PatternExpr comp)

instance Compilable (SexpParser DoCmd) [Sexp] DoCmd where
    comp =
        (macro "<-" (liftA2 DoAssign comp comp) <+>
         liftA1 DoCmdExpr comp)

instance Compilable (SexpParser Where) [Sexp] Where where
    comp = macro "where" (liftA1 Where comp)

instance Compilable (SexpParser Pattern) [Sexp] Pattern where
    comp = 
        (liftA1 ListPattern (macro "List" comp)   <+>
         liftA1 TuplePattern (macro "Tuple" comp) <+>
         liftA1 StringPattern (macro "Str" (many comp >>> arr unwords))  <+>
         liftA1 CallPattern comp)

instance Compilable (SexpParser Call) [Sexp] Call where
    comp = 
        (liftA1 ConstOpCall (comp >>> failUnless isOp)      <+>
         liftA1 ConstCall   (comp <+> compNode comp) <+>
         compNode (liftA2 
                    OpFoldCall 
                    (comp >>> failUnless isOp)
                    comp)                                         <+>
         compNode (liftA1 FunCall comp))

instance Compilable (SexpParser Instance) [Sexp] Instance where
    comp = macro "instance" (liftA2 Instance (compNode comp) comp)