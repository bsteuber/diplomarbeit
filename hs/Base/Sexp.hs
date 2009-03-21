module Sexp where

data Sexp = Sexp { label    :: String,
                   children :: [Sexp] }
            deriving (Eq)

labelEq name = (name==) . label

symbol s = Sexp s []