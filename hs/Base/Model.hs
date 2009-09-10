{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
module Model where
import Control.Exception
import Control.Arrow
import Util
import Arrows

compileStr :: (Compiler String a, Executable x a b, Compiler b String) => x -> IOFun String String
compileStr f = runKleisli $ compile >>> toIO f >>> compile

compiler :: (Compiler String a, Executable x a b, Compiler b String) => x -> IO ()
compiler f = do
  input <- getContents
  output <- compileStr f input
  putStr output

testCompiler :: (Compiler String a, Executable x a b, Compiler b String) => String -> x -> [(String, String)] -> IO ()
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


