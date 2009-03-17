module Eater where 

import Prelude hiding (catch)
import Data.List
import Control.Monad
import qualified Control.Exception as E
import Util
import Sexp

newtype Eater a b = Eater { eat :: Trans [a] ([a], b) }

instance Monad (Eater a) where
    return x = Eater (\cs -> Success (cs, x))
    c >>= f  = Eater (\cs -> case eat c cs of
                                  Error msg -> Error msg
                                  Success (cs1, x) -> eat (f x) cs1)

eatNothing :: Eater a ()
eatNothing = return ()

eatFail :: Eater a b
eatFail = Eater (const (Error "EatFail called"))

trans2eater :: (Trans a b) -> Eater a b
trans2eater sc =  Eater app
    where app []     = Error "trans2eater: empty stream"
          app (c:cs) = do res <- sc c
                          return (cs, res)

eater2trans :: (Show a) => Eater a b -> (Trans [a] b)
eater2trans eater stream = 
    case eat eater stream of
      Success ([], res) -> Success res
      Success (rest, _) -> Error $ "eater2trans: Eater could not eat " ++ show rest
      Error msg         -> Error msg

eater2singleTrans :: (Show a) => Eater a b -> (Trans a b)
eater2singleTrans eater sexp = 
    case eat eater [sexp] of
      Success ([], res) -> Success res
      Success (rest, _) -> Error $ "eater2singleTrans: Eater could not eat " ++ show rest
      Error msg         -> Error msg

eatEmpty :: Eater a ()
eatEmpty = Eater app
    where app [] = Success ([], ())
          app _  = Error "eatEmpty: non-empty stream"
    
eatOr :: Eater a b -> Eater a b -> Eater a b
eatOr c1 c2 = Eater (\cs -> case eat c1 cs of
                             Error msg -> eat c2 cs
                             r       -> r)

eatOrs :: [Eater a b] -> Eater a b
eatOrs = foldl' eatOr eatFail

eatMaybe :: Eater a b -> Eater a [b] 
eatMaybe c = Eater (\cs -> case eat c cs of
                            Error msg -> Success (cs, [])
                            Success (rest, out) -> Success (rest, [out]))

eatMany :: Eater a b -> Eater a [b]
eatMany c = eatOr (eatMany1 c) (return [])

eatMany1 :: Eater a b -> Eater a [b]
eatMany1 c = do fst  <- c
                rest <- eatMany c
                return (fst : rest)


skipMany :: Eater a b -> Eater a ()
skipMany e = do eatMany e
                return ()

skipMany1 :: Eater a b -> Eater a ()
skipMany1 e = do eatMany1 e
                 return ()

eatAll :: Eater a b -> Eater a [b]
eatAll e = (eatOr
            (liftM (const []) eatEmpty)
            (liftM2 (:) e (eatAll e)))

eatCompletely :: Eater a b -> Eater a b
eatCompletely e = do
  res <- e
  eatEmpty
  return res

eatAny :: Eater a a
eatAny = eatWhen (const True) id

eatWhen :: (a -> Bool) -> (a -> b) -> Eater a b
eatWhen p f = Eater eat
    where eat (x:xs) | p x       = Success (xs, f x)
                     | otherwise = Error "eatWhen: predicate not satisfied"
          eat _ = Error "eatWhen: empty stream"

token :: (Eq a) => a -> Eater a a
token x = eatWhen (== x) id

stream :: (Eq a) => [a] -> Eater a [a]
stream xs = sequence (map token xs)

eatNot :: Eater a b -> Eater a ()
eatNot e =
    let f stream = case eat e stream of
                     Error _   -> Success (stream, ())
                     Success _ -> Error "Eater for eatNot succeeded"
    in
      Eater f

oneOf :: (Eq a) => [a] -> Eater a a
oneOf xs = eatOrs (map token xs)
    
noneOf :: (Eq a) => [a] -> Eater a a
noneOf xs = do eatNot (oneOf xs)
               eatAny