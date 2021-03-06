{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances, IncoherentInstances #-}
module Parser where
import Prelude hiding (id, (.), take, fail, Functor)
import Data.Typeable
import Data.IORef
import Control.Exception (Exception, throw)
import Control.Category
import Control.Arrow
import Util
import Arrows
import FailFunctor
import StateFunctor
import Model

newtype ParseFunctor t ar a b = P {runP :: StateFunctor [t] (FailFunctor (FailFunctor ar)) a b }

type FunParser   t a b = ParseFunctor t (->) a b
type IOParser    t a b = ParseFunctor t IOArrow a b
type ExecParser ar t b = ParseFunctor t ar () b
type ExecFunParser t b = ExecParser (->) t b
type ExecIOParser  t b = ExecParser IOArrow t b

ioToParser :: (a -> IO b) -> IOParser t a b
ioToParser = lift . Kleisli

instance (ArrowChoice ar) => Category (ParseFunctor t ar) where
    id = arr id
    (P f) . (P g) = P (f . g)

instance (ArrowChoice ar) => Arrow (ParseFunctor t ar) where
    arr = P . arr
    first (P f) = P (first f)

instance (ArrowChoice ar) => ArrowChoice (ParseFunctor t ar) where
    left (P f) = P (left f)

instance (ArrowChoice ar) => ArrowZero (ParseFunctor t ar) where
    zeroArrow = P zeroArrow

instance (ArrowChoice ar) => ArrowFail (ParseFunctor t ar) where
    fail = P fail

instance (ArrowChoice ar) => ArrowPlus (ParseFunctor t ar) where
    P f <+> P g = P (f <+> g)

instance (ArrowChoice ar, ArrowApply ar) => ArrowApply (ParseFunctor t ar) where
    app = P $ (arr runP *** id) >>> app

instance (ArrowChoice ar) => Functor (ParseFunctor t ar) ar where
    lift = P . lift . lift . lift

instance (ArrowChoice ar) => ArrowState [t] (ParseFunctor t ar) where
    get = P get
    put = P put

instance (Show t, ArrowFail ar, ArrowChoice ar, Executable (ar [t] (Failable a)) [t] (Failable a)) => 
    Executable (ParseFunctor t ar () a) [t] a where
        toIO = toIO . execParser

instance (Show t, ArrowFail ar, ArrowChoice ar, Executable (ar [t] (Failable [a])) [t] (Failable [a]), 
          Compilable (ParseFunctor t ar () a) [t] a) =>
    Compilable (ParseFunctor t ar () [a]) [t] [a] where
        comp = many comp

instance (Show t, ArrowFail ar, ArrowChoice ar, 
          Executable (ar [t] (Failable (Maybe a))) [t] (Failable (Maybe a)), 
          Compilable (ParseFunctor t ar () a) [t] a) => 
    Compilable (ParseFunctor t ar () (Maybe a)) [t] (Maybe a) where
        comp = optional comp

execParser :: (Show t, ArrowFail ar, ArrowChoice ar) => ParseFunctor t ar () a -> FailFunctor ar [t] a
execParser p = (execFail . execState . runP) (p >>> empty) 

forceParser :: (ArrowFail ar, ArrowChoice ar) => ExecParser ar t a -> ExecParser ar t a
forceParser = P . StateF . forceFail . runStateF . runP

applyParser :: (ArrowChoice ar) => ParseFunctor t ar a [t] -> ParseFunctor t ar a b -> ParseFunctor t ar a b
applyParser getInner (P parseInner) = (id &&& getInner) >>> P (withState parseInner)


debug :: String -> IOParser t a a
debug msg = if debugging then 
                lift $ Kleisli $ \ x -> do
                  debugPrint msg
                  return x
            else
                id

debugIORef :: (Show a) => IORef a -> IOParser t b b
debugIORef ref = if debugging then 
                     lift $ Kleisli $ \ x -> do
                       val <- readIORef ref
                       debugPrint (show val)
                       return x
                 else
                     id

debugArrow :: IOParser t String ()
debugArrow = if debugging then 
                 lift $ Kleisli $ \ msg -> do
                   debugPrint msg
                   return ()
             else
                 voidArrow

debugNextToken :: (Show t) => IOParser t a a
debugNextToken = (take >>> arr show >>> debugArrow >>> constArrow "" >>> fail) <+> id

debugWithNextToken str = debug (str ++ ": [") >>> debugNextToken >>> debug "]"

empty :: (Show t, ArrowChoice ar) => ParseFunctor t ar a a
empty = (get &&& id) >>> (ifArrow
                          (arr $ null . fst)
                          (arr snd)
                          (arr fst >>> arr errorMsg >>> fail))
    where errorMsg x = "Empty stream expected: " ++ show x

take :: (ArrowChoice ar) => ParseFunctor t ar a t
take = get >>> (ifArrow
                 (arr $ not . null)
                 (skip (arr tail >>> put) >>> arr head)
                 (constArrow "Nonempty stream expected" >>> fail))

push :: (ArrowChoice ar) => ParseFunctor t ar t ()
push = (id &&& get) >>> arr (uncurry (:)) >>> put

takeWhen :: (ArrowChoice ar) => (t -> Bool) -> (t -> String) -> ParseFunctor t ar a t
takeWhen pred errorMsg =
    take >>> (ifArrow
               (arr pred)
               id
               (arr errorMsg >>> fail))

eq :: (Eq t, Show t, ArrowChoice ar) => t -> ParseFunctor t ar a t
eq x = takeWhen (==x) errorMsg
    where errorMsg y = "\nToken " ++ show y ++ "\nShould be " ++ show x

notEq :: (Eq t, Show t, ArrowChoice ar) => t -> ParseFunctor t ar a t
notEq x = takeWhen (/= x) (const $ "\nToken should not be " ++ show x)

member :: (Eq t, Show t, ArrowChoice ar) => [t] -> ParseFunctor t ar a t
member xs = takeWhen (`elem` xs) errorMsg
    where errorMsg y = "\nElement " ++ show y ++ "\nShould be member of " ++ show xs

notMember :: (Eq t, Show t, ArrowChoice ar) => [t] -> ParseFunctor t ar a t
notMember xs = takeWhen (not . (`elem` xs)) errorMsg
    where errorMsg y = "\nElement " ++ show y ++ "\nShould not be member of " ++ show xs

streamEq :: (Eq t, Show t, ArrowChoice ar) => [t] -> ParseFunctor t ar a [t]
streamEq = foldArrows . map eq
