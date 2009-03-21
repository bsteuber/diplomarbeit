module Compiler where
import Prelude hiding (id, (.), fail, Functor)
import Control.Category
import Control.Arrow
import Util
import Arrows
import Sexp
import Parser
import Reader
import Writer

type SexpParser = Parser Sexp

takeSexpWhen :: (String -> Bool) -> (String -> String) -> SexpParser a b -> SexpParser a b
takeSexpWhen pred errorMsg parseInner = 
    (id &&& token) >>> (ifArrow
                        (arr (snd >>> label >>> pred))
                        (second (arr children) >>> runParser parseInner)
                        (arr (snd >>> label >>> errorMsg) >>> fail))

macro :: String -> SexpParser a b -> SexpParser a b
macro name = (takeSexpWhen 
              (== name)
              (\ lbl -> "Expected symbol " ++ name ++ "\nGot " ++ lbl))

compile :: SexpParser () a -> Sexp -> IO a
compile p sexp = execParser p [sexp]

compileStr :: (Show a) => SexpParser () a -> String -> IO String
compileStr p str = do
  sexp <- readSexp str
  res <- compile p sexp
  return $ show res

testMacro :: String -> SexpParser () Sexp -> [(String, String)] -> IO ()
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
                                 res     <- compile mac sexp
                                 tgtSexp <- readSexp tgt
                                 (if res == tgtSexp then
                                     recTest cases errorOccured
                                  else
                                      do putStrLn $ ("Compiler test failed for " ++
                                                     show sexp  ++ ":\n  Expected:\n" ++ 
                                                     show tgtSexp ++ "\n  Got:\n" ++ show res)
                                         recTest cases True)

                
