module Eater where 
import Prelude hiding (id, (.), fail)
import Data.List
import Control.Category
import Control.Arrow
import Control.Monad hiding (fail)
import Util
import Failable

newtype (ArrowChoice ar) => Eater ar s a b =
    E { runE :: Failable ar (a, s) (b, s) }

tupleSwap :: ((a, b), c) -> ((a, c), b)
tupleSwap    ((x, y), z) = ((x, z), y)

instance (ArrowChoice ar) => Arrow (Eater ar s) where
    arr fun     = E $ arr $ first fun
    first (E f) = E $ tupleSwap ^>> first f >>^ tupleSwap

instance (ArrowChoice ar) => Category (Eater ar s) where
    id        = arr id
    E g . E f = E $ f >>> g      

instance (ArrowChoice ar) => ArrowZero (Eater ar s) where
    zeroArrow = E zeroArrow

instance (ArrowChoice ar) => ArrowPlus (Eater ar s) where
    E f <+> E g = E $ f <+> g

-- instance (ArrowChoice ar) => ArrowFail (Eater ar s) where
--     fail = E $ fail

getState :: (a, s) -> s
getState = snd

getInput :: (a, s) -> a
getInput = fst

setState :: s -> (a, s) -> (a, s)
setState state (x, _) = (x, state)

drop :: (ArrowChoice ar) => Eater ar [s] a a
drop = E $ arr $ setState []

getRest :: (ArrowChoice ar) => Eater ar [s] a [s] 
getRest = E $ arr $ \ (_, stream) -> (stream, [])

-- trans2eater :: (Trans a b) -> Eater a b
-- trans2eater sc =  Eater app
--     where app []     = Error "trans2eater: empty stream"
--           app (c:cs) = do res <- sc c
--                           return (cs, res)

-- eater2trans :: (Show a) => Eater a b -> (Trans [a] b)
-- eater2trans eater stream = 
--     case eat eater stream of
--       Success ([], res) -> Success res
--       Success (rest, _) -> Error $ "eater2trans: Eater could not eat " ++ show rest
--       Error msg         -> Error msg

-- eater2singleTrans :: (Show a) => Eater a b -> (Trans a b)
-- eater2singleTrans eater sexp = 
--     case eat eater [sexp] of
--       Success ([], res) -> Success res
--       Success (rest, _) -> Error $ "eater2singleTrans: Eater could not eat " ++ show rest
--       Error msg         -> Error msg

-- eatEmpty :: Eater a ()
-- eatEmpty = Eater app
--     where app [] = Success ([], ())
--           app _  = Error "eatEmpty: non-empty stream"
    
-- eatOr :: Eater a b -> Eater a b -> Eater a b
-- eatOr c1 c2 = Eater (\cs -> case eat c1 cs of
--                              Error msg -> eat c2 cs
--                              r       -> r)

-- eatOrs :: [Eater a b] -> Eater a b
-- eatOrs = foldl' eatOr eatFail

-- eatMaybe :: Eater a b -> Eater a [b] 
-- eatMaybe c = Eater (\cs -> case eat c cs of
--                             Error msg -> Success (cs, [])
--                             Success (rest, out) -> Success (rest, [out]))

-- eatMany :: Eater a b -> Eater a [b]
-- eatMany c = eatOr (eatMany1 c) (return [])

-- eatMany1 :: Eater a b -> Eater a [b]
-- eatMany1 c = do fst  <- c
--                 rest <- eatMany c
--                 return (fst : rest)


-- skipMany :: Eater a b -> Eater a ()
-- skipMany e = do eatMany e
--                 return ()

-- skipMany1 :: Eater a b -> Eater a ()
-- skipMany1 e = do eatMany1 e
--                  return ()

-- eatAll :: Eater a b -> Eater a [b]
-- eatAll e = (eatOr
--             (liftM (const []) eatEmpty)
--             (liftM2 (:) e (eatAll e)))

-- eatCompletely :: Eater a b -> Eater a b
-- eatCompletely e = do
--   res <- e
--   eatEmpty
--   return res

-- eatAny :: Eater a a
-- eatAny = eatWhen (const True) id

-- eatWhen :: (a -> Bool) -> (a -> b) -> Eater a b
-- eatWhen p f = Eater eat
--     where eat (x:xs) | p x       = Success (xs, f x)
--                      | otherwise = Error "eatWhen: predicate not satisfied"
--           eat _ = Error "eatWhen: empty stream"

-- token :: (Eq a) => a -> Eater a a
-- token x = eatWhen (== x) id

-- stream :: (Eq a) => [a] -> Eater a [a]
-- stream xs = sequence (map token xs)

-- eatNot :: Eater a b -> Eater a ()
-- eatNot e =
--     let f stream = case eat e stream of
--                      Error _   -> Success (stream, ())
--                      Success _ -> Error "Eater for eatNot succeeded"
--     in
--       Eater f

-- oneOf :: (Eq a) => [a] -> Eater a a
-- oneOf xs = eatOrs (map token xs)
    
-- noneOf :: (Eq a) => [a] -> Eater a a
-- noneOf xs = do eatNot (oneOf xs)
--                eatAny