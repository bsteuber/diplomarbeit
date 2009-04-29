{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module FailFunctor where
import Prelude hiding (id, (.), fail, Functor)
import Control.Category
import Control.Arrow
import Util
import Arrows

newtype FailFunctor ar a b = FailF {runFailF :: ar a (Failable b)}

type FailFun = FailFunctor (->)

execFail :: (ArrowChoice ar, ArrowFail ar) => FailFunctor ar a b -> ar a b
execFail (FailF f) = f >>> (fail ||| id)

forceFail :: (ArrowChoice ar, ArrowFail ar) => FailFunctor ar a b -> FailFunctor ar a b
forceFail = lift . execFail

instance (Arrow ar) => Functor (FailFunctor ar) ar where
    lift f = FailF $ f >>^ Right

instance (ArrowChoice ar) => Arrow (FailFunctor ar) where
    arr f = lift $ arr f
    first (FailF f) =
        FailF $ first f >>^ tuple2either
            where tuple2either (Left msg, y)  = Left msg
                  tuple2either (Right res, y) = Right (res, y)

instance (ArrowChoice ar) => Category (FailFunctor ar) where
  FailF g . FailF f =
      FailF $ f >>> (arr Left ||| g)
  id = lift id

instance (ArrowChoice ar) => ArrowChoice (FailFunctor ar) where
    left (FailF f) =
        FailF $ left f >>^ flipEither
            where flipEither (Left (Left x))  = Left x
                  flipEither (Left (Right y)) = Right (Left y)
                  flipEither (Right z)        = Right (Right z)

instance (ArrowChoice ar) => ArrowZero (FailFunctor ar) where
    zeroArrow = constArrow "Failure" >>> fail

instance (ArrowChoice ar) => ArrowPlus (FailFunctor ar) where
    FailF f <+> FailF g = FailF $ (f &&& g) >>^ tupleOr
        where tupleOr :: (Either a b, Either a b) -> Either a b
              tupleOr (Left  _, y) = y
              tupleOr (x@(Right _), _) = x

instance (ArrowChoice ar, ArrowApply ar) => ArrowApply (FailFunctor ar) where
    app = FailF $ first (arr runFailF) >>> app

instance (ArrowChoice ar) => ArrowFail (FailFunctor ar) where
    fail = FailF (arr Left)

instance (ArrowChoice ar, ArrowState s ar) => ArrowState s (FailFunctor ar) where
    get = lift get
    put = lift put

instance (Executable (ar a (Failable b)) a (Failable b)) => Executable (FailFunctor ar a b) a b where
    toIO (FailF f) = toIO f >>> (fail ||| id)