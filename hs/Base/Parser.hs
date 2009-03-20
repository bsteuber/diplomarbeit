{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Parser where
import Prelude hiding (id, (.), fail, Functor)
import Data.List
import Control.Category
import Control.Arrow
import qualified Control.Monad as M
import Util
import Arrows
import FailFunctor
import StateFunctor

newtype Program a b = Prog { runProg :: Kleisli IO a b }
    deriving (Category, Arrow, ArrowChoice, ArrowApply)

instance ArrowFail Program where
    fail = Prog $ Kleisli $ M.fail

execProg :: Program a b -> a -> IO b
execProg = runKleisli . runProg

newtype Parser t a b = P {runP :: FailFunctor (StateFunctor [t] Program) a b }
    deriving (Category, Arrow, ArrowChoice, ArrowZero, ArrowPlus, ArrowFail)

instance ArrowState [t] (Parser t) where
    get = P get
    put = P put                                 

execParser :: Parser t () a -> [t] -> IO a
execParser = execProg . execState . execFail . runP    

getList :: Parser t a (Either [t] [t])
getList = get >>^ f
    where f xs@[] = Left xs
          f xs    = Right xs

takeFirst :: Parser t a b -> Parser t (a, [t]) b -> Parser t a b
takeFirst handleEmpty handleElt =
    (getList &&& id) >>> arr prepare >>> (handleEmpty ||| ((handleElt &&& (snd ^>> tail ^>> put)) >>^ fst))
    where prepare (Left _, x)       = Left x
          prepare (Right stream, x) = Right (x, stream)

empty :: (Show t) => Parser t a a
empty = takeFirst id (arr (("Empty stream expected: "++) . show . snd) >>> fail)

token :: Parser t a t
token = takeFirst (constArrow "Nonempty stream expected" >>> fail) (arr (head . snd))

 -- testEmpty >>> arr prepareChoice >>> FailF (arr Right ||| arr (errorMsg >>> Left))
 --    where testEmpty = (liftFail fetch >>> test (arr null)) &&& id
 --          errorMsg stream = "Empty stream expected: " ++ show stream
 --          prepareChoice (Left _, x)  = Left x
 --          prepareChoice (Right s, _) = Right s

-- token :: Parser t a t
-- token = P $ testEmpty >>> arr prepareChoice >>> FailF (constArrow (Left "Unexpected empty stream") ||| arr Right)
--     where testEmpty = (liftFail fetch >>> test (arr null)) &&& id
--           prepareChoice (Left _, _)      = Left ()
--           prepareChoice (Right (t:_), _) = Right t


-- eatAll e = (eatOr
--             (liftM (const []) empty)
--             (liftM2 (:) e (eatAll e)))

-- eatCompletely :: StateFunctor a b -> StateFunctor a b
-- eatCompletely e = do
--   res <- e
--   empty
--   return res

-- eatAny :: StateFunctor a a
-- eatAny = eatWhen (const True) id

-- eatWhen :: (a -> Bool) -> (a -> b) -> StateFunctor a b
-- eatWhen p f = StateFunctor eat
--     where eat (x:xs) | p x       = Success (xs, f x)
--                      | otherwise = Error "eatWhen: predicate not satisfied"
--           eat _ = Error "eatWhen: empty stream"

-- token :: (Eq a) => a -> StateFunctor a a
-- token x = eatWhen (== x) id

-- stream :: (Eq a) => [a] -> StateFunctor a [a]
-- stream xs = sequence (map token xs)

-- eatNot :: StateFunctor a b -> StateFunctor a ()
-- eatNot e =
--     let f stream = case eat e stream of
--                      Error _   -> Success (stream, ())
--                      Success _ -> Error "StateFunctor for eatNot succeeded"
--     in
--       StateFunctor f

-- oneOf :: (Eq a) => [a] -> StateFunctor a a
-- oneOf xs = eatOrs (map token xs)
    
-- noneOf :: (Eq a) => [a] -> StateFunctor a a
-- noneOf xs = do eatNot (oneOf xs)
--                eatAny