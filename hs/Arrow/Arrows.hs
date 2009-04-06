{-# OPTIONS -fglasgow-exts #-}
module Arrows where
import Prelude hiding (id, (.), fail, Functor)
import Data.List
import Control.Category
import Control.Arrow
import Control.Monad (liftM)
import Util

class Functor f where
    lift :: (Arrow ar) => ar a b -> f ar a b

class (Arrow ar) => ArrowFail ar where
    fail :: ar String a

class (Arrow ar) => ArrowState s ar | ar -> s where
    get :: ar a s
    put :: ar s ()

type IOFun a b = a -> IO b

type IOArrow = Kleisli IO

class Executable x a b | x -> a b where
    toIO :: x -> IOArrow a b

instance Executable (IOArrow a b) a b where
    toIO = id

instance Executable (a -> b) a b where
    toIO f = Kleisli (return . f)

class (Executable x a b) => Compilable x a b | a b -> x where
    comp :: x

compile :: (Compilable x a b) => IOArrow a b
compile = toIO comp

liftA0 c = constArrow c
liftA1 fun f = f >>^ fun
liftA2 fun f g = (f &&& g) >>^ uncurry fun
liftA3 fun f g h = (f &&& g &&& h) >>^ \ (x, (y, z)) -> fun x y z

constArrow :: (Arrow ar) => b -> ar a b
constArrow = arr . const

voidArrow :: (Arrow ar) => ar a ()
voidArrow = constArrow ()

nilArrow :: (Arrow ar) => ar a [b]
nilArrow = constArrow []

consArrow :: (Arrow ar) => ar a b -> ar a [b] -> ar a [b]
consArrow eltArrow listArrow = (eltArrow &&& listArrow) >>> arr (uncurry (:))

foldArrows :: (Arrow ar) => [ar a b] -> ar a [b]
foldArrows = foldr consArrow nilArrow

afoldr :: (ArrowChoice ar) => ar (a, b) b -> ar () b -> ar [a] b
afoldr comb init = arr uncons >>> (init ||| (second (afoldr comb init) >>> comb)) 
    where uncons []     = Left ()
          uncons (x:xs) = Right (x, xs)

amap :: (ArrowChoice ar) => ar a b -> ar [a] [b]
amap f = afoldr (first f >>> arr (uncurry (:))) nilArrow

ifArrow :: (ArrowChoice ar) => ar a Bool -> ar a b -> ar a b -> ar a b
ifArrow testArrow thenArrow elseArrow =
    ((testArrow >>> arr bool2either) &&& id) >>> arr eitherRes >>> (thenArrow ||| elseArrow)
        where eitherRes (Left  _, x) = Left  x
              eitherRes (Right _, x) = Right x

failUnless :: (ArrowChoice ar, ArrowFail ar) => (a -> Bool) -> ar a a
failUnless pred = ifArrow (arr pred) id (constArrow "failUnless failed" >>> fail)

optional :: (ArrowPlus ar) => ar a b -> ar a (Maybe b)
optional f = (f >>^ Just) <+> constArrow Nothing

many :: (ArrowPlus ar) => ar a b -> ar a [b]
many f = many1 f <+> nilArrow

many1 :: (ArrowPlus ar) => ar a b -> ar a [b]
many1 f = consArrow f (many f)

skip :: (Arrow ar) => ar a b -> ar a a
skip f = (f &&& id) >>^ snd

skipMany :: (ArrowPlus ar) => ar a b -> ar a a
skipMany = skip . many

skipMany1 :: (ArrowPlus ar) => ar a b -> ar a a
skipMany1 = skip . many1

sepBy :: (ArrowPlus ar) => ar a c -> ar a b -> ar a [b]
sepBy sep item = optional (consArrow item (many (skip sep >>> item))) >>^ unMaybeList

-- test :: (Arrow ar) => ar a Bool -> ar a (Either a a)
-- test f = (f &&& id) >>> (arr $ \ (b, x) -> if b then Left x else Right x)