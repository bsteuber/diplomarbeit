module StateFunctor where 
import Prelude hiding (id, (.), fail)
import Data.List
import Control.Category
import Control.Arrow
import Control.Monad hiding (fail)
import Util

newtype StateFunctor s ar a b =
    SF { runSF :: ar (a, s) (b, s) }

tupleSwap :: ((a, b), c) -> ((a, c), b)
tupleSwap    ((x, y), z) = ((x, z), y)

instance (Arrow ar) => Arrow (StateFunctor s ar) where
    arr fun     = SF $ arr $ first fun
    first (SF f) = SF $ tupleSwap ^>> first f >>^ tupleSwap

instance (Arrow ar) => Category (StateFunctor s ar) where
    id        = arr id
    SF g . SF f = SF $ f >>> g      

instance (ArrowZero ar) => ArrowZero (StateFunctor s ar) where
    zeroArrow = SF zeroArrow

instance (ArrowPlus ar) => ArrowPlus (StateFunctor s ar) where
    SF f <+> SF g = SF $ f <+> g

instance (ArrowChoice ar) => ArrowChoice (StateFunctor s ar) where
    left (SF f) = SF (arr (\ (z, s) -> case z of
                                         Left b  -> Left (b, s)
                                         Right c -> Right (c, s)) >>>
                      ((f >>> first (arr Left)) ||| first (arr Right)))

fetch :: (Arrow ar) => StateFunctor s ar a s
fetch = SF $ arr $ \ (_, s) -> (s, s)

store :: (Arrow ar) => StateFunctor s ar s ()
store = SF $ arr $ \ (x, _) -> ((), x)

-- trans2eater :: (Trans a b) -> StateFunctor a b
-- trans2eater sc =  StateFunctor app
--     where app []     = Error "trans2eater: empty stream"
--           app (c:cs) = do res <- sc c
--                           return (cs, res)

-- eater2trans :: (Show a) => StateFunctor a b -> (Trans [a] b)
-- eater2trans eater stream = 
--     case eat eater stream of
--       Success ([], res) -> Success res
--       Success (rest, _) -> Error $ "eater2trans: StateFunctor could not eat " ++ show rest
--       Error msg         -> Error msg

-- eater2singleTrans :: (Show a) => StateFunctor a b -> (Trans a b)
-- eater2singleTrans eater sexp = 
--     case eat eater [sexp] of
--       Success ([], res) -> Success res
--       Success (rest, _) -> Error $ "eater2singleTrans: StateFunctor could not eat " ++ show rest
--       Error msg         -> Error msg
