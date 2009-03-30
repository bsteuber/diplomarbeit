module Util where

class OfString a where
    ofString :: String -> a

class ToString a where
    toString :: a -> String

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