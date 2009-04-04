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

instance Executable (Program a b) a b where
    toIO = runProg

ioToParser :: IOArrow a b -> Parser t a b
ioToParser = P . lift . lift . Prog

newtype Parser t a b = P {runP :: StateFunctor [t] (FailFunctor Program) a b }
    deriving (Category, Arrow, ArrowChoice, ArrowZero, ArrowPlus, ArrowFail)

instance ArrowState [t] (Parser t) where
    get = P get
    put = P put

instance (Show t) => Executable (Parser t () a) [t] a where
    toIO = execParser

instance (Show t, Compilable (Parser t () a) [t] a) => Compilable (Parser t () [a]) [t] [a] where
    comp = many comp

instance (Show t, Compilable (Parser t () a) [t] a) => Compilable (Parser t () (Maybe a)) [t] (Maybe a) where
    comp = optional comp

execParser :: (Show t) => Parser t () a -> IOArrow [t] a
execParser p = (runProg . execFail . execState . runP) (p >>> empty) 

applyParser :: Parser s a [s] -> Parser s a b -> Parser s a b
applyParser getInner (P parseInner) = (id &&& getInner) >>> P (withState parseInner)

debug :: String -> Parser s a a
debug msg = P $ lift $ lift $ Prog $ Kleisli $ \ x -> do
              putStrLn msg
              return x

empty :: (Show t) => Parser t a a
empty = (get &&& id) >>> (ifArrow
                          (arr $ null . fst)
                          (arr snd)
                          (arr fst >>> arr errorMsg >>> fail))
    where errorMsg x = "Empty stream expected: " ++ show x

take :: Parser t a t
take = get >>> (ifArrow
                 (arr $ not . null)
                 (skip (arr tail >>> put) >>> arr head)
                 (constArrow "Nonempty stream expected" >>> fail))

takeWhen :: (t -> Bool) -> (t -> String) -> Parser t a t
takeWhen pred errorMsg =
    take >>> (ifArrow
               (arr pred)
               id
               (arr errorMsg >>> fail))

eq :: (Eq t, Show t) => t -> Parser t a t
eq x = takeWhen (==x) errorMsg
    where errorMsg y = "\nToken " ++ show y ++ "\nShould be " ++ show x

notEq :: (Eq t, Show t) => t -> Parser t a t
notEq x = takeWhen (/= x) (const $ "\nToken should not be " ++ show x)

member :: (Eq t, Show t) => [t] -> Parser t a t
member xs = takeWhen (`elem` xs) errorMsg
    where errorMsg y = "\nElement " ++ show y ++ "\nShould be member of " ++ show xs

notMember :: (Eq t, Show t) => [t] -> Parser t a t
notMember xs = takeWhen (not . (`elem` xs)) errorMsg
    where errorMsg y = "\nElement " ++ show y ++ "\nShould not be member of " ++ show xs

streamEq :: (Eq t, Show t) => [t] -> Parser t a [t]
streamEq = foldArrows . map eq
