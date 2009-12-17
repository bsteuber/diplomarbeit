module Haskell where

data Haskell = Haskell (Maybe Module) [Toplevel]

data Module = Module ModuleName (Maybe Export) [Import]
type ModuleName = String
data Export = Export [String]
data Import = Import ModuleName ImportArgs
data ImportArgs = Simple
                | Qualified ModuleAbbrev
                | Only [FunctionName]
                | Hiding [FunctionName]
type ModuleAbbrev = String
type FunctionName = String


data Toplevel = TopHasType HasType
              | TopDef Def
              | TopTypeAlias TypeAlias 
              | TopData Data
              | TopClass Class
              | TopInstance Instance

data HasType = HasType Expr (Maybe TypeDependancy) Type

data Type = NormalType String
          | ListType Type
          | TupleType [Type]
          | FunType [Type]
          | ParamType String [Type]

data Def = Def Pattern Expr (Maybe Where)

data Expr = LambdaExpr  [Pattern] Expr
          | DoExpr      [DoCmd]
          | TypeExpr    HasType
          | PatternExpr Pattern

data DoCmd = DoAssign Pattern Expr
           | DoCmdExpr Expr

data Where = Where [Toplevel]

data Pattern = ListPattern [Pattern]
             | TuplePattern [Pattern]
             | ConsPattern [Pattern]
             | StringPattern String
             | CallPattern Call                          

data Call = ConstCall String
          | ConstOpCall String
          | FunCall [Expr]
          | OpFoldCall String [Expr]

data TypeAlias = TypeAlias Type Type

data Data = Data Type [Constructor]

data Constructor = Constructor String [Type]

data Class = Class (Maybe TypeDependancy) Type Where

data Instance = Instance (Maybe TypeDependancy) Type Where

data TypeDependancy = TypeDependancy [Type]