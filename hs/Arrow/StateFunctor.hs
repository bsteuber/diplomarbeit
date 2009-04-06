{-# OPTIONS -fglasgow-exts #-}
module StateFunctor where
import Prelude hiding (id, (.), fail, Functor)
import Data.List
import Control.Category
import Control.Arrow
import Util
import Arrows

newtype StateFunctor s ar a b =
    StateF { runStateF :: ar (a, s) (b, s) }

execState :: (Arrow ar) => StateFunctor s ar () a -> ar s a
execState (StateF f) = arr (\ s -> ((), s)) >>> f >>> arr fst

withState :: (Arrow ar) => StateFunctor s ar a b -> StateFunctor s ar (a, s) b
withState (StateF f) = StateF (first (f >>^ fst))

instance (Arrow ar) => Functor (StateFunctor s ar) ar where
    lift = StateF . first

instance (Arrow ar) => Arrow (StateFunctor s ar) where
    arr f = lift (arr f)
    first (StateF f) = StateF $ tupleSwap ^>> first f >>^ tupleSwap
        where tupleSwap ((x, y), z) = ((x, z), y)

instance (Arrow ar) => Category (StateFunctor s ar) where
    id = lift id
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
  fail = lift fail

instance (ArrowApply ar) => ArrowApply (StateFunctor s ar) where
    app = StateF $ arr (\ ((StateF f, x), s) -> (f, (x, s))) >>> app

instance (Arrow ar) => ArrowState s (StateFunctor s ar)
    where
      get = StateF $ arr $ \ (_, state) -> (state, state)
      put = StateF $ arr $ \ (state, _) -> ((), state)