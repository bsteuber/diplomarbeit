module ModelLang where
import qualified Haskell as H

data Model        = Model String [Compiler] [TypeDef]
data Compiler     = ParseCompiler {compilerModel :: String}
                  | FunCompiler   {compilerModel ::String}
data TypeDef      = TypeDef String [ConstrDef]
data ConstrDef    = ConstrDef String SlotDef [CompDef]
data SlotDef      = SlotDef [Slot]
data Slot         = Slot String H.Type
data CompDef      = NoCompDef
                  | ParseCompDef ParseSpecial
                  | HsCompDef H.Expr
data ParseSpecial = It
                  | Mac String CompDef