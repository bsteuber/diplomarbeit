{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
module ModelLang where
import Prelude hiding (lines, words)
import Control.Arrow
import Util
import Arrows
import Model
import Parser
import Sexp
import Haskell
import Sexp2Haskell

---------
--Model--
---------

data Model = Model String [Compiler] [ModelType]

instance Compilable (SexpParser Model) [Sexp] Model where
    comp = macro "model" (liftA3 Model comp (optMacro "compilers" comp) comp)

-- instance Compilable (Model -> Haskell) Model Haskell where
--     comp (Model name comps modelTypes) =
--         (Haskell 
--          (Just (Module
--                 name
--                 Nothing
--                 ([ Import "Prelude"       (Hiding ["lines", "words"])
--                  , Import "Control.Arrow" Simple
--                  , Import "Util"          Simple
--                  , Import "Arrows"        Simple
--                  , Import "Model"         Simple
--                  , Import "Parser"        Simple
--                  ] ++ map comp comps)))
--          (concat $ map comp modelTypes))

instance Compilable (SexpGen Model Haskell) Model Haskell where
    comp = SexpGen $ const $ (unSexpGen comp (42::Int) :: GenSexp Haskell)

instance Compilable (SexpGen Int Haskell) Int Haskell where
    comp = SexpGen $ GenSexp . single . symbol . show

data Compiler = ParseCompiler {compilerModel :: String}
              | FunCompiler   {compilerModel ::String}

instance Compilable (SexpParser Compiler) [Sexp] Compiler where
    comp = macro "parser" (liftA1 ParseCompiler comp) <+>
           macro "fun"    (liftA1 FunCompiler comp)           

instance Compilable (Compiler -> Import) Compiler Import where
    comp c = Import (compilerModel c) Simple

data ModelType = ModelType String [ConstrDef]

instance Compilable (SexpParser ModelType) [Sexp] ModelType where
    comp = compNode (liftA2 ModelType comp comp)


-- instance Compilable (ModelType -> Haskell) ModelType Haskell where
--     comp (ModelType name constrDefs) = 

data ConstrDef = ConstrDef String SlotDef [CompDef]

instance Compilable (SexpParser ConstrDef) [Sexp] ConstrDef where
    comp = compNode (liftA3 ConstrDef comp comp comp)


data SlotDef = SlotDef [Slot]

instance Compilable (SexpParser SlotDef) [Sexp] SlotDef where
    comp = macro "slots" (liftA1 SlotDef comp)


data Slot = Slot String Type

instance Compilable (SexpParser Slot) [Sexp] Slot where
    comp = compNode (liftA2 Slot comp comp)


data CompDef = NoCompDef
             | ParseCompDef ParseSpecial
             | HsCompDef Expr

instance Compilable (SexpParser CompDef) [Sexp] CompDef where
    comp = (symbolMacro "noComp" >>> liftA0 NoCompDef) <+>
           liftA1 ParseCompDef comp <+>
           liftA1 HsCompDef comp


data ParseSpecial = It
                  | Mac String CompDef

instance Compilable (SexpParser ParseSpecial) [Sexp] ParseSpecial where
    comp = (symbolMacro "it" >>> liftA0 It) <+>
           macro "mac" (liftA2 Mac comp comp)

-- instance Compilable (Model -> Haskell) Model Haskell where
--     comp (Model name mayComps typeDefs) =

-- instance Compilable (Model -> Haskell) Model Haskell where
--     comp (Model name mayComps typeDefs) =

-- instance Compilable (Model -> Haskell) Model Haskell where
--     comp (Model name mayComps typeDefs) =

-- instance Compilable (Model -> Haskell) Model Haskell where
--     comp (Model name mayComps typeDefs) =

-- instance Compilable (Model -> Haskell) Model Haskell where
--     comp (Model name mayComps typeDefs) =

-- instance Compilable (Model -> Haskell) Model Haskell where
--     comp (Model name mayComps typeDefs) =

