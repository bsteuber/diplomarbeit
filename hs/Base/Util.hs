{-# OPTIONS -fglasgow-exts #-}
module Util where
import Data.Typeable
import Control.Exception

debug = True

type Failable a = Either String a

data MagicError = MagicError String
                  deriving (Typeable)

instance Show MagicError where
    show (MagicError msg) = msg

instance Exception MagicError

magicError = throw . MagicError

interleave :: a -> [a] -> [a]
interleave i (x:y:xs) = x : i : interleave i (y:xs)
interleave _ xs = xs

copy :: Int -> a -> [a]
copy i = take i . repeat

(|>) :: a -> (a -> b) -> b
(|>) = flip id

single :: a -> [a]
single = return

unSingle :: (Show a) => [a] -> a
unSingle [x] = x
unSingle xs  = error $ "Single element expected: " ++ show xs

bool2either :: Bool -> Either () ()
bool2either b = if b then Left () else Right ()

unMaybeList :: Maybe [a] -> [a]
unMaybeList  Nothing  = []
unMaybeList (Just xs) = xs

maybe2list :: Maybe a -> [a]
maybe2list (Just x) = [x]
maybe2list _        = []