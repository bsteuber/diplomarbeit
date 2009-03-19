module Types where
  

-- Eater

newtype Eater ar a b = Eater { runEater :: Failable ar [a] (b, [a]) }

-- instance (Arrow ar) => Arrow (Eater ar) where
--     arr fun = Eater $ arr $ \ cs -> (x, cs)
--     first (Eater f) =
--         Eater

-- Sexp

-- data Sexp = Symbol {symbolName   :: String}
--           | Node   {nodeName     :: String,
--                     nodeChildren :: Stream}
--     deriving Eq

-- type Stream = [Sexp]

-- type SymbolTrans a = String -> Failing a
-- type SymbolPred = String -> Bool


-- type SexpEater a = Eater Sexp a
-- type Macro = SexpEater Sexp

