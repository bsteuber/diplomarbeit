module FailFunctor where
import Prelude hiding (id, (.), fail)
import Control.Category
import Control.Arrow
import Control.Monad hiding (fail)
import Util

newtype FailFunctor ar a b = FailF {runFailF :: ar a (Either String b)}

class ArrowFail ar where
    fail :: String -> ar a b

liftFail :: (Arrow ar) => ar a b -> FailFunctor ar a b
liftFail f = FailF $ f >>^ Right

execFail :: (ArrowChoice ar, ArrowApply ar, ArrowFail ar) => FailFunctor ar a b -> ar a b
execFail (FailF f) = f >>> (handleFailure ||| id)
    where handleFailure = arr (fail &&& const ()) >>> app

instance (ArrowChoice ar) => Arrow (FailFunctor ar) where
    arr f = liftFail $ arr f
    first (FailF f) = 
        FailF $ first f >>^ tuple2either
            where tuple2either (Left msg, y)  = Left msg
                  tuple2either (Right res, y) = Right (res, y)

instance (ArrowChoice ar) => Category (FailFunctor ar) where
  FailF g . FailF f =
      FailF $ f >>> (arr Left ||| g)
  id = liftFail id

instance (ArrowChoice ar) => ArrowChoice (FailFunctor ar) where
    left (FailF f) = 
        FailF $ left f >>^ flipEither
            where flipEither (Left (Left x))  = Left x
                  flipEither (Left (Right y)) = Right (Left y)
                  flipEither (Right z)        = Right (Right z)

instance (ArrowChoice ar) => ArrowZero (FailFunctor ar) where
    zeroArrow = fail "Failure"

instance (ArrowChoice ar) => ArrowPlus (FailFunctor ar) where
    FailF f <+> FailF g = FailF $ (f &&& g) >>^ tupleOr
        where tupleOr :: (Either a b, Either a b) -> Either a b
              tupleOr (Left  _, y) = y
              tupleOr (x@(Right _), _) = x

instance (ArrowChoice ar) => ArrowFail (FailFunctor ar) where
    fail msg = FailF $ arr $ const $ Left msg

instance (ArrowChoice ar, ArrowApply ar) => ArrowApply (FailFunctor ar) where
    app = FailF $ first (arr runFailF) >>> app

test :: (Arrow ar) => ar a Bool -> ar a (Either a a)
test f = (f &&& id) >>> (arr $ \ (b, x) -> if b then Left x else Right x)