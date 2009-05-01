module Haskell where

type ModuleName = String
type FunctionName = String
type ModuleAbbrev = String

data Haskell = Haskell (Maybe Module) [Toplevel]

data Module = Module ModuleName (Maybe Export) [Import]

data Export = Export [String]

data ImportArgs = Simple
                | Qualified ModuleAbbrev
                | Only [FunctionName]
                | Hiding [FunctionName]

data Import = Import ModuleName ImportArgs

data Toplevel = TopTypeDef TypeDef
              | TopDef Def
              | TopData Data
              | TopClass Class
              | TopInstance Instance

data TypeDef = TypeDef Expr (Maybe TypeDependancy) Type

data Type = NormalType String
          | ListType Type
          | TupleType [Type]
          | FunType [Type]
          | ParamType String [Type]

data Def = Def Pattern Expr (Maybe Where)

data Expr = LambdaExpr  [Pattern] Expr
          | DoExpr      [DoCmd]
          | TypeExpr    TypeDef
          | PatternExpr Pattern

data DoCmd = DoAssign Pattern Expr
           | DoCmdExpr Expr

data Where = Where [Toplevel]

data Pattern = ListPattern [Pattern]
             | TuplePattern [Pattern]
             | StringPattern String
             | CallPattern Call

data Call = ConstCall String
          | ConstOpCall String
          | FunCall [Expr]
          | OpFoldCall String [Expr]

data Data = Data Type [Constructor]

data Constructor = Constructor String [Type]

data Class = Class (Maybe TypeDependancy) Type Where

data Instance = Instance (Maybe TypeDependancy) Type Where

data TypeDependancy = TypeDependancy [Type]