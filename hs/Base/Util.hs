module Util where

interleave :: a -> [a] -> [a]
interleave i (x:y:xs) = x : i : interleave i (y:xs)
interleave _ xs = xs

copy :: Int -> a -> [a]
copy i = take i . repeat

(|>) :: a -> (a -> b) -> b
(|>) = flip id

single :: a -> [a]
single = return
