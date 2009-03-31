module Model where
import Control.Arrow
import Util
import Arrows
import Sexp

class OfString a where
    ofString :: IOArrow String a

class ToString a where
    toString :: IOArrow a String

class OfSexp a where
    ofSexp :: IOArrow Sexp a

class ToSexp a where
    toSexp :: IOArrow a Sexp

compileStr :: (OfString a, ToString b) => IOArrow a b -> IOFun String String
compileStr f = runKleisli $ ofString >>> f >>> toString

compiler :: (OfString a, ToString b) => IOArrow a b -> IO ()
compiler f = do
  input <- getContents
  output <- compileStr f input
  putStr output

testCompiler :: (OfString a, ToString b) => String -> IOArrow a b -> [(String, String)] -> IO ()
testCompiler name comp testCases = do
    putStrLn $ "Testing macro " ++ name
    recTest testCases False
        where
          recTest [] False = do putStrLn "Hooray! Tests passed."
                                return ()
          recTest [] True = do putStrLn "Sorry, there were errors."
                               return ()
          recTest ((src, tgt):cases) errorOccured = do
                                 res <- compileStr comp src
                                 (if res == tgt then
                                     recTest cases errorOccured
                                  else
                                      do (putStrLn
                                          ("Compiler test failed for " ++
                                           src  ++ ":\n  Expected:\n" ++
                                           tgt ++ "\n  Got:\n" ++ res))
                                         recTest cases True)


