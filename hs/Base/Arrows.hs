{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}
module Arrows where
import Prelude hiding (id, (.), fail, Functor)
import Data.List
import Control.Category
import Control.Arrow
import Util

class Functor f where
    lift :: (Arrow ar) => ar a b -> f ar a b

class (Arrow ar) => ArrowFail ar where
    fail :: ar String a

class (Arrow ar) => ArrowState s ar | ar -> s where
    get :: ar a s
    put :: ar s () 

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

ifArrow :: (ArrowChoice ar) => ar a Bool -> ar a b -> ar a b -> ar a b
ifArrow testArrow thenArrow elseArrow =
    ((testArrow >>> arr bool2either) &&& id) >>> arr eitherRes >>> (thenArrow ||| elseArrow)
        where eitherRes (Left  _, x) = Left  x
              eitherRes (Right _, x) = Right x    

optional :: (ArrowPlus ar) => ar a b -> ar a [b]
optional f = (f >>^ single) <+> nilArrow

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
sepBy sep item = optional (consArrow item (many (skip sep >>> item))) >>^ concat