{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}
module Model where
import Util
import Arrows
import Sexp

class (ArrowIO ar) => OfString ar a | a -> ar where
    ofString :: ar String a

class (ArrowIO ar) => ToString ar a | a -> ar where
    toString :: ar a String

class (ArrowIO ar) => OfSexp ar a | a -> ar where
    ofSexp :: ar Sexp a

class (ArrowIO ar) => ToSexp ar a | a -> ar where
    toSexp :: ar a Sexp

ofStringIO :: (OfString ar a) => String -> IO a
ofStringIO = toIO ofString

toStringIO :: (ToString ar a) => a -> IO String
toStringIO = toIO toString

ofSexpIO :: (OfSexp ar a) => Sexp -> IO a
ofSexpIO = toIO ofSexp

toSexpIO :: (ToSexp ar a) => a -> IO Sexp
toSexpIO = toIO toSexp

compileStr :: (ArrowIO ar, OfString ar1 a, ToString ar2 b) => ar a b -> String -> IO String
compileStr f inStr = do
  input  <- ofStringIO inStr
  output <- toIO f input
  toStringIO output

compiler :: (ArrowIO ar, OfString ar1 a, ToString ar2 b) => ar a b -> IO ()
compiler f = do
  input <- getContents
  output <- compileStr f input
  putStr output

