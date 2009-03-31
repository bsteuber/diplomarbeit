{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}
module Compiler where
import Prelude hiding (id, (.), take, fail, Functor)
import Control.Category
import Control.Arrow
import Control.Monad (liftM)
import System.Environment (getArgs)
import System.IO
import Util
import Arrows
import Sexp
import Parser
import Reader
import Writer

type SexpParser = Parser Sexp

takeSexp :: SexpParser a (String, [Sexp])
takeSexp = take >>> arr sexp2tuple

takeSymbol :: SexpParser a String
takeSymbol =
    takeSexp >>> (ifArrow
                  (arr $ snd >>> null)
                  (arr fst)
                  (arr (tuple2sexp >>> show >>> ("Symbol expected: "++)) >>> fail))

procSexp :: SexpParser TupleSexp TupleSexp -> SexpParser a Sexp
procSexp procTuple = takeSexp >>> procTuple >>> arr tuple2sexp

takeSexpWhen :: (String -> Bool) -> (String -> String) -> SexpParser (a, String) b -> SexpParser a b
takeSexpWhen pred errorMsg parseInner =
    (id &&& takeSexp) >>> (ifArrow
                           (arr (snd >>> fst >>> pred))
                           (arr (\ (x, (lbl, stream)) -> ((x, lbl), stream)) >>> runParser parseInner)
                           (arr (snd >>> fst >>> errorMsg) >>> fail))

takeAnySexp = takeSexpWhen (const True) (const "")

looseMacro :: String -> SexpParser a b -> SexpParser a b
looseMacro name parseInner = (takeSexpWhen
                              (== name)
                              (\ lbl -> "Expected symbol " ++ name ++ "\nGot " ++ lbl)
                              (arr fst >>> parseInner))

macro :: String -> SexpParser a b -> SexpParser a b
macro name parseInner = looseMacro name (parseInner >>> empty)

symbolMacro name retVal = macro name (constArrow retVal)

-- compile :: SexpParser () a -> Sexp -> IO a
-- compile p sexp = execParser p [sexp]

testMacro :: (ToString a) => String -> SexpParser () a -> [(String, String)] -> IO ()
testMacro name mac testCases = do
    putStrLn $ "Testing macro " ++ name
    recTest testCases False
        where
          recTest [] False = do putStrLn "Hooray! Tests passed."
                                return ()
          recTest [] True = do putStrLn "Sorry, there were errors."
                               return ()
          recTest ((src, tgt):cases) errorOccured = do
                                 sexp    <- readSexp src
                                 comp    <- compile mac sexp
                                 let res = showScode comp
                                 (if res == tgt then
                                     recTest cases errorOccured
                                  else
                                      do (putStrLn
                                          ("Compiler test failed for " ++
                                           showScode sexp  ++ ":\n  Expected:\n" ++
                                           tgt ++ "\n  Got:\n" ++ res))
                                         recTest cases True)


