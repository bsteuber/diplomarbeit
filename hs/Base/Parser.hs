{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
module Parser (arr, Parser, execParser, empty, token, when, eq, notEq, member, notMember, streamEq) where
import Prelude hiding (id, (.), fail, Functor)
import Data.Typeable
import Control.Exception (Exception, throw)
import Control.Category
import Control.Arrow
import Util
import Arrows
import FailFunctor
import StateFunctor

newtype Program a b = Prog { runProg :: Kleisli IO a b }
    deriving (Category, Arrow, ArrowChoice, ArrowApply)

instance ArrowFail Program where
    fail = Prog $ Kleisli $ error . ("Error in Parser: "++)

execProg :: Program a b -> a -> IO b
execProg = runKleisli . runProg

newtype Parser t a b = P {runP :: StateFunctor [t] (FailFunctor Program) a b }
    deriving (Category, Arrow, ArrowChoice, ArrowZero, ArrowPlus, ArrowFail)

instance ArrowState [t] (Parser t) where
    get = P get
    put = P put                                 

execParser :: Parser t () a -> [t] -> IO a
execParser = execProg . execFail . execState . runP    

empty :: (Show t) => Parser t a ()
empty = (get &&& id) >>> (ifArrow 
                          (arr $ null . fst)
                          voidArrow
                          (arr errorMsg >>> fail))
        where errorMsg (stream, _) = "Empty stream expected: " ++ (show stream)

token :: Parser t a t
token = get >>> (ifArrow 
                 (arr $ not . null)
                 (skip (arr tail >>> put) >>> arr head) 
                 (constArrow "Nonempty stream expected" >>> fail))

when :: (t -> Bool) -> (t -> String) -> Parser t a t
when pred errorMsg =
    token >>> (ifArrow 
               (arr pred)
               id
               (arr errorMsg >>> fail))

eq :: (Eq t, Show t) => t -> Parser t a t
eq x = when (==x) errorMsg
    where errorMsg y = "\nToken " ++ show y ++ "\nShould be " ++ show x        

notEq :: (Eq t, Show t) => t -> Parser t a t
notEq x = when (/= x) errorMsg
    where errorMsg _ = "\nToken should not be " ++ show x

member :: (Eq t, Show t) => [t] -> Parser t a t
member xs = when (`elem` xs) errorMsg
    where errorMsg y = "\nElement " ++ show y ++ "\nShould be member of " ++ show xs

notMember :: (Eq t, Show t) => [t] -> Parser t a t
notMember xs = when (not . (`elem` xs)) errorMsg
    where errorMsg y = "\nElement " ++ show y ++ "\nShould not be member of " ++ show xs

streamEq :: (Eq t, Show t) => [t] -> Parser t a [t]
streamEq = foldArrows . map eq
