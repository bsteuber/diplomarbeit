module Sexp where
import Util

data Sexp = Symbol { symbolName :: String }
          | Node   { children   :: [Sexp] }
            deriving (Eq)

sexp2either (Symbol s)   = Left s
sexp2either (Node sexps) = Right sexps

label :: Sexp -> String
label = symbolName . head . children

symbol = Symbol
node = Node

isNode (Node _)   = True
isNode (Symbol _) = False

isSymbol (Node _)   = False
isSymbol (Symbol _) = True

-- lbl children = Node (Symbol lbl : children)
-- singleNode lbl = node lbl . single

labelEq name = (name==) . label