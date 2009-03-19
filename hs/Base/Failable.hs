module Failable where
import Prelude hiding (id, (.), fail)
import Control.Category
import Control.Arrow
import Control.Monad hiding (fail)
import Util


tuple2either (Left msg, y)  = Left msg
tuple2either (Right res, y) = Right (res, y)

tupleOr :: (Either a b, Either a b) -> Either a b
tupleOr (Left  _, y) = y
tupleOr (x@(Right _), _) = x

flipEither :: Either (Either a b) c -> Either a (Either b c)
flipEither (Left (Left x))  = Left x
flipEither (Left (Right y)) = Right (Left y)
flipEither (Right z)        = Right (Right z)

newtype (ArrowChoice ar) => Failable ar a b = F {runF :: ar a (Either String b)}

class ArrowFail ar where
    fail :: String -> ar () a

-- force :: (ArrowChoice ar, ArrowApply ar, ArrowFail ar) => Failable ar a b -> ar a b
-- force (F f) = f >>> ((arr fail) >>^ (\ g -> (g, ())) >>> app ) ||| id)

instance (ArrowChoice ar) => ArrowFail (Failable ar) where
    fail msg = F $ arr $ const $ Left msg

instance (ArrowChoice ar) => Arrow (Failable ar) where
    arr fun = F $ arr (Right . fun)
    first (F f) = 
        F $ first f >>^ tuple2either

instance (ArrowChoice ar) => Category (Failable ar) where
  F g . F f =
      F $ f >>> (arr Left ||| g)
  id = arr id

instance (ArrowChoice ar) => ArrowChoice (Failable ar) where
    left (F f) = 
        F $ left f >>^ flipEither

instance (ArrowChoice ar) => ArrowZero (Failable ar) where
    zeroArrow = F $ arr $ const $ Left "Failure"

instance (ArrowChoice ar) => ArrowPlus (Failable ar) where
    F f <+> F g = F $ (f &&& g) >>^ tupleOr
