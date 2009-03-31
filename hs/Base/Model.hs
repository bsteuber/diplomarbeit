module Model where
import Util
import Arrows
import Sexp

class OfString a where
    ofString :: IOFun String a

class ToString IOFun a where
    toString :: IOFun a String

class OfSexp IOFun a where
    ofSexp :: IOFun Sexp a

class ToSexp IOFun a where
    toSexp :: IOFun a Sexp

compileStr :: (OfString a, ToString b) => IOFun a b -> String -> IO String
compileStr f inStr = do
  input  <- ofString inStr
  output <- toIO f input
  toString output

compiler :: (OfString a, ToString b) => IOFun a b -> IO ()
compiler f = do
  input <- getContents
  output <- compileStr f input
  putStr output

