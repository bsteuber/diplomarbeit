module Arrows where
import Prelude hiding (id, (.), fail)
import Data.List
import Control.Category
import Control.Arrow
import Util

constArrow :: (Arrow ar) => b -> ar a b
constArrow = arr . const

voidArrow :: (Arrow ar) => ar a ()
voidArrow = constArrow ()

nilArrow :: (Arrow ar) => ar a [b]
nilArrow = constArrow []

consArrow :: (Arrow ar) => ar (a, [a]) [a]
consArrow = arr $ uncurry (:)

optional :: (ArrowPlus ar) => ar a b -> ar a [b]
optional f = (f >>^ single) <+> nilArrow

many :: (ArrowPlus ar) => ar a b -> ar a [b]
many f = many1 f <+> nilArrow

many1 :: (ArrowPlus ar) => ar a b -> ar a [b]
many1 f = (f &&& many f) >>> consArrow

skip :: (Arrow ar) => ar a b -> ar a ()
skip f = f >>> voidArrow

skipMany :: (ArrowPlus ar) => ar a b -> ar a ()
skipMany = skip . many

skipMany1 :: (ArrowPlus ar) => ar a b -> ar a ()
skipMany1 = skip . many1

sepBy :: (ArrowPlus ar) => ar a () -> ar a b -> ar a [b]
sepBy sep item = (optional $ (item &&& (many $ (sep &&& item) >>^ snd)) >>> consArrow) >>^ concat

