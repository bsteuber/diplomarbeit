module Sexp where

data Sexp = Sexp { label    :: String,
                   children :: [Sexp] }

labelEq name = (name==) . label

symbol s = Sexp s []