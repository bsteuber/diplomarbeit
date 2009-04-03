{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances, IncoherentInstances #-}
module Parser where
import Prelude hiding (id, (.), take, fail, Functor)
import Data.Typeable
import Control.Exception (Exception, throw)
import Control.Category
import Control.Arrow
import Util
import Arrows
import FailFunctor
import StateFunctor
import Model

newtype Program a b = Prog { runProg :: Kleisli IO a b }
    deriving (Category, Arrow, ArrowChoice, ArrowApply)

instance ArrowFail Program where
    fail = Prog $ Kleisli $ error . ("Error in Parser: "++)

instance ArrowIO Program where
    toIO = runProg

ioToParser :: IOArrow a b -> Parser t a b
ioToParser = P . lift . lift . Prog

newtype Parser t a b = P {runP :: StateFunctor [t] (FailFunctor Program) a b }
    deriving (Category, Arrow, ArrowChoice, ArrowZero, ArrowPlus, ArrowFail)

class (Compilable t String) => Parsable t a where
    parse :: Parser t () a

instance (Parsable t a) => Parsable t [a] where
    parse = many parse

instance (Parsable t a) => Parsable t (Maybe a) where
    parse = optional parse

instance (Parsable t a) => Compilable [t] a where
    compile = execParser parse

instance (Parsable t a) => Compilable t a where
    compile = arr single >>> execParser parse

instance ArrowState [t] (Parser t) where
    get = P get
    put = P put

execParser :: (Compilable t String) => Parser t () a -> IOArrow [t] a
execParser p = (runProg . execFail . execState . runP) (p >>> empty) 

applyParser :: Parser s a [s] -> Parser s a b -> Parser s a b
applyParser getInner (P parseInner) = (id &&& getInner) >>> P (withState parseInner)

debug :: String -> Parser s a a
debug msg = P $ lift $ lift $ Prog $ Kleisli $ \ x -> do
              putStrLn msg
              return x

empty :: (Compilable t String) => Parser t a a
empty = (get &&& id) >>> (ifArrow
                          (arr $ null . fst)
                          (arr snd)
                          (arr fst >>> ioToParser compile >>> arr errorMsg >>> fail))
    where errorMsg str = "Empty stream expected: " ++ str

take :: Parser t a t
take = get >>> (ifArrow
                 (arr $ not . null)
                 (skip (arr tail >>> put) >>> arr head)
                 (constArrow "Nonempty stream expected" >>> fail))

takeWhen :: (t -> Bool) -> (Parser t t String) -> Parser t a t
takeWhen pred errorMsg =
    take >>> (ifArrow
               (arr pred)
               id
               (errorMsg >>> fail))

eq :: (Eq t, Compilable t String) => t -> Parser t a t
eq x = takeWhen (==x) (ioToParser ((constArrow x >>> compile) &&& compile) >>> arr errorMsg)
    where errorMsg (x, y) = "\nToken " ++ y ++ "\nShould be " ++ x

notEq :: (Eq t, Compilable t String) => t -> Parser t a t
notEq x = takeWhen (/= x) (ioToParser compile >>> arr ("\nToken should not be "++))

member :: (Eq t, Compilable t String) => [t] -> Parser t a t
member xs = takeWhen (`elem` xs) (ioToParser ((constArrow xs >>> compile) &&& compile) >>> arr errorMsg)
    where errorMsg (xs, y) = "\nElement " ++ y ++ "\nShould be member of " ++ xs

notMember :: (Eq t, Compilable t String) => [t] -> Parser t a t
notMember xs = takeWhen (not . (`elem` xs)) (ioToParser ((constArrow xs >>> compile) &&& compile) >>> arr errorMsg)
    where errorMsg (xs, y) = "\nElement " ++ y ++ "\nShould not be member of " ++ xs

streamEq :: (Eq t, Compilable t String) => [t] -> Parser t a [t]
streamEq = foldArrows . map eq
