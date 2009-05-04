{-# OPTIONS -fglasgow-exts #-}
module Sexp2ModelLang where
import Prelude hiding (lines, words)
import Control.Arrow
import Util
import Arrows
import Model
import Parser
import Sexp
import ModelLang
import qualified Haskell as H

instance Compilable (SexpParser Model) [Sexp] Model where
    comp = macro "model" (liftA3 Model comp (optMacro "compilers" comp) comp)

instance Compilable (SexpParser Compiler) [Sexp] Compiler where
    comp = macro "parser" (liftA1 ParseCompiler comp) <+>
           macro "fun"    (liftA1 FunCompiler comp)           

instance Compilable (SexpParser TypeDef) [Sexp] TypeDef where
    comp = compNode (liftA2 TypeDef comp comp)

instance Compilable (SexpParser ConstrDef) [Sexp] ConstrDef where
    comp = compNode (liftA3 ConstrDef comp comp comp)

instance Compilable (SexpParser SlotDef) [Sexp] SlotDef where
    comp = macro "slots" (liftA1 SlotDef comp)

instance Compilable (SexpParser Slot) [Sexp] Slot where
    comp = compNode (liftA2 Slot comp comp)

instance Compilable (SexpParser CompDef) [Sexp] CompDef where
    comp = (symbolMacro "noComp" >>> liftA0 NoCompDef) <+>
           liftA1 ParseCompDef comp <+>
           liftA1 HsCompDef comp

instance Compilable (SexpParser ParseSpecial) [Sexp] ParseSpecial where
    comp = (symbolMacro "it" >>> liftA0 It) <+>
           macro "mac" (liftA2 Mac comp comp)



