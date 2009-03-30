module Sexp where
import Util

data Sexp = Sexp { label    :: String,
                   children :: [Sexp] }
            deriving (Eq)

symbol s = Sexp s []
node = Sexp
singleNode lbl = Sexp lbl . single

labelEq name = (name==) . label

type TupleSexp = (String, [Sexp])

sexp2tuple (Sexp lbl cs) = (lbl, cs)
tuple2sexp (lbl, cs) = Sexp lbl cs