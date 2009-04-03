{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
module Model where
import Control.Arrow
import Util
import Arrows
import Sexp

class Compilable a b where
    compile :: IOArrow a b

class FunComp a b where
    comp :: a -> b

instance (Compilable a String) => Compilable [a] String where
    compile = amap compile >>> arr concat

instance Compilable Char String where
    compile = toIO show

compileStr :: (Compilable String a, Compilable b String) => IOArrow a b -> IOFun String String
compileStr f = runKleisli $ compile >>> f >>> compile

compiler :: (Compilable String a, Compilable b String) => IOArrow a b -> IO ()
compiler f = do
  input <- getContents
  output <- compileStr f input
  putStr output

testCompiler :: (Compilable String a, Compilable b String) => String -> IOArrow a b -> [(String, String)] -> IO ()
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


