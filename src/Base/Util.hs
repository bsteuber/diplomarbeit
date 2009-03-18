{-# OPTIONS_GHC -XDeriveDataTypeable #-}

module Util where

import qualified Control.Exception as E
import Data.Typeable
import Control.Monad

interleave :: a -> [a] -> [a]
interleave i (x:y:xs) = x : i : interleave i (y:xs)
interleave _ xs = xs

copy :: Int -> a -> [a]
copy i = take i . repeat

(|>) :: a -> (a -> b) -> b
(|>) = flip id


data MagiclException = 
    ReaderException String  |
    CompileException String |
    TestException String
    deriving (Eq, Typeable)

instance Show MagiclException where
    show s = 
        "Error for " ++
        (case s of
           ReaderException  msg -> "Parser: " ++ msg
           CompileException msg -> "Compiler: " ++ msg
           TestException    msg -> "Test: " ++ msg
        )

instance E.Exception MagiclException

data Failing a = Error String
               | Success a
                 deriving (Show)

instance Monad Failing where
    return x = Success x
    (>>=) m fn = case m of
                   Error s -> Error s
                   Success x -> fn x

type Trans a b = a -> Failing b


trans2fun :: (String -> MagiclException) -> Trans a b -> (a -> b)
trans2fun exc f x = case f x of 
                    Success res -> res
                    Error msg   -> exc msg |> E.throw

fun2trans f x = Success (f x)

single :: a -> [a]
single = return