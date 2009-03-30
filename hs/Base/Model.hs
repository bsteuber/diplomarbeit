{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}
module Model where
import Util
import Arrows

class (ArrowIO ar) => OfString ar a | a -> ar where
    ofString :: ar String a

class (ArrowIO ar) => ToString ar a | a -> ar where
    toString :: ar a String

compileStr :: (ArrowIO ar, OfString ar1 a, ToString ar2 b) => ar a b -> String -> IO String
compileStr f inStr = do
  input  <- toIO ofString inStr
  output <- toIO f input
  toIO toString output

compiler :: (ArrowIO ar, OfString ar1 a, ToString ar2 b) => ar a b -> IO ()
compiler f = do
  input <- getContents
  output <- compileStr f input
  putStr output

