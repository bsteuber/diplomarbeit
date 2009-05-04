{-# OPTIONS -fglasgow-exts #-}
module ModelLang2Code where
import Prelude hiding (lines, words)
import Control.Arrow
import Util
import Arrows
import Model
import Parser
import Code
import ModelLang
import Haskell

instance Compilable (Model -> Haskell) Model Haskell where
    comp (Model name comps typeDefs) =
        (Haskell 
         (Just (Module
                name
                Nothing
                ([ Import "Prelude"       (Hiding ["lines", "words"])
                 , Import "Control.Arrow" Simple
                 , Import "Util"          Simple
                 , Import "Arrows"        Simple
                 , Import "Model"         Simple
                 , Import "Parser"        Simple
                 ] ++ map ((\m -> Import m Simple) . compilerModel) comps)))
         (map comp typeDefs))
    

instance Compilable (TypeDef -> Haskell) TypeDef Haskell where
    comp (TypeDef name  typeDefs) =

instance Compilable (Model -> Haskell) Model Haskell where
    comp (Model name mayComps typeDefs) =

instance Compilable (Model -> Haskell) Model Haskell where
    comp (Model name mayComps typeDefs) =

instance Compilable (Model -> Haskell) Model Haskell where
    comp (Model name mayComps typeDefs) =

instance Compilable (Model -> Haskell) Model Haskell where
    comp (Model name mayComps typeDefs) =

instance Compilable (Model -> Haskell) Model Haskell where
    comp (Model name mayComps typeDefs) =

instance Compilable (Model -> Haskell) Model Haskell where
    comp (Model name mayComps typeDefs) =

instance Compilable (Model -> Haskell) Model Haskell where
    comp (Model name mayComps typeDefs) =

