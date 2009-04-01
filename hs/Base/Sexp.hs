module Sexp where
import Util

data Sexp = Symbol { symbolName :: String }
          | Node   { children   :: [Sexp] }
            deriving (Eq)

label :: Sexp -> String
label = symbolName . head . children

symbol = Symbol
node = Node

-- lbl children = Node (Symbol lbl : children)
-- singleNode lbl = node lbl . single

labelEq name = (name==) . label