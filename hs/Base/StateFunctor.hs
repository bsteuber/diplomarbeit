module StateFunctor where 
import Prelude hiding (id, (.), fail)
import Data.List
import Control.Category
import Control.Arrow
import Control.Monad hiding (fail)
import Util
import FailFunctor

newtype StateFunctor s ar a b =
    StateF { runStateF :: ar (a, s) (b, s) }

liftState :: (Arrow ar) => ar a b -> StateFunctor s ar a b
liftState f = StateF $ first f

execState :: (Arrow ar) => StateFunctor s ar () a -> ar s a
execState (StateF f) = arr (\ s -> ((), s)) >>> f >>> arr fst

instance (Arrow ar) => Arrow (StateFunctor s ar) where
    arr f = liftState (arr f)
    first (StateF f) = StateF $ tupleSwap ^>> first f >>^ tupleSwap
        where tupleSwap ((x, y), z) = ((x, z), y)

instance (Arrow ar) => Category (StateFunctor s ar) where
    id = liftState id
    StateF g . StateF f = StateF $ f >>> g      

instance (ArrowZero ar) => ArrowZero (StateFunctor s ar) where
    zeroArrow = StateF zeroArrow

instance (ArrowPlus ar) => ArrowPlus (StateFunctor s ar) where
    StateF f <+> StateF g = StateF $ f <+> g

instance (ArrowChoice ar) => ArrowChoice (StateFunctor s ar) where
    left (StateF f) = StateF (arr (\ (z, s) -> case z of
                                         Left b  -> Left (b, s)
                                         Right c -> Right (c, s)) >>>
                      ((f >>> first (arr Left)) ||| first (arr Right)))

instance (Arrow ar, ArrowFail ar) => ArrowFail (StateFunctor s ar) where
  fail = liftState . fail

instance (ArrowApply ar) => ArrowApply (StateFunctor s ar) where
    app = StateF $ arr (\ ((StateF f, x), s) -> (f, (x, s))) >>> app

fetch :: (Arrow ar) => StateFunctor s ar a s
fetch = StateF $ arr $ \ (_, s) -> (s, s)

store :: (Arrow ar) => StateFunctor s ar s ()
store = StateF $ arr $ \ (x, _) -> ((), x)

fetchCons :: (ArrowChoice ar) => StateFunctor [t] ar () b -> StateFunctor [t] ar t b -> StateFunctor [t] ar a b
fetchCons handleEmpty handleElt =
    fetch >>> (test (arr null) &&& id) >>> arr prepareChoice >>> (handleEmpty ||| ((handleElt *** store) >>^ fst))
    where prepareChoice (Left _, _)      = Left ()
          prepareChoice (Right (token : rest), _) = Right (token, rest)


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
