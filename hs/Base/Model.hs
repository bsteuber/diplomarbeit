{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
module Model where
import Control.Exception
import Control.Arrow
import Util
import Arrows

compileStr :: (Compilable x1 String a, Executable x2 a b, Compilable x3 b String) => x2 -> IOFun String String
compileStr f = runKleisli $ compile >>> toIO f >>> compile

compiler :: (Compilable x1 String a, Executable x2 a b, Compilable x3 b String) => x2 -> IO ()
compiler f = do
  input <- getContents
  output <- compileStr f input
  putStr output

testCompiler :: (Compilable x1 String a, Executable x2 a b, Compilable x3 b String) => String -> x2 -> [(String, String)] -> IO ()
testCompiler name comp testCases = do
    putStrLn $ "Testing macro " ++ name
    recTest testCases False
        where
          recTest [] False = do putStrLn "Hooray! Tests passed."
                                return ()
          recTest [] True = do putStrLn "Sorry, there were errors."
                               return ()
          recTest ((src, tgt):cases) errorOccured = do
                                 errorOrRes :: Either MagicError String <- try $ compileStr comp src
                                 (case errorOrRes of
                                    Left e -> printError $ show e
                                    Right res -> if res == tgt then
                                                    recTest cases errorOccured
                                                else
                                                    printError $ "Expected:\n" ++ tgt ++ "\n  Got:\n" ++ res)
              where printError msg = do putStrLn $ "When testing " ++ src ++ "\n" ++ msg
                                        recTest cases True


