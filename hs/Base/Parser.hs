{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Parser where
import Prelude hiding (id, (.), fail)
import Data.List
import Control.Category
import Control.Arrow
import qualified Control.Monad as M
import Util
import FailFunctor
import StateFunctor

newtype Program a b = Prog { runProg :: Kleisli IO a b }
    deriving (Category, Arrow, ArrowChoice, ArrowApply)

instance ArrowFail Program where
    fail = Prog . Kleisli . const . M.fail

execProg :: Program a b -> a -> IO b
execProg = runKleisli . runProg

newtype Parser t a b = P {runP :: FailFunctor (StateFunctor [t] Program) a b }
    deriving (Category, Arrow, ArrowChoice, ArrowZero, ArrowPlus)

execParser :: Parser t () a -> [t] -> IO a
execParser = execProg . execState . execFail . runP    

empty :: Parser t a ()
empty = P $ liftFail fetch >>> test (arr null) >>> ((arr (const ())) ||| fail "empty: non-empty stream")



-- eatMaybe :: StateFunctor a b -> StateFunctor a [b] 
-- eatMaybe c = StateFunctor (\cs -> case eat c cs of
--                             Error msg -> Success (cs, [])
--                             Success (rest, out) -> Success (rest, [out]))

-- eatMany :: StateFunctor a b -> StateFunctor a [b]
-- eatMany c = eatOr (eatMany1 c) (return [])

-- eatMany1 :: StateFunctor a b -> StateFunctor a [b]
-- eatMany1 c = do fst  <- c
--                 rest <- eatMany c
--                 return (fst : rest)


-- skipMany :: StateFunctor a b -> StateFunctor a ()
-- skipMany e = do eatMany e
--                 return ()

-- skipMany1 :: StateFunctor a b -> StateFunctor a ()
-- skipMany1 e = do eatMany1 e
--                  return ()

-- eatAll :: StateFunctor a b -> StateFunctor a [b]
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