module TestComp2Haskell where
import Parser
import Compiler
import qualified Haskell2Code as H
import Comp2Haskell


quoteCases = [ ( "(' 42)"
               , "(symbol \"42\")" )
             , ( "(' (+ 1 2))"
               , "(Sexp \"+\" ([(symbol \"1\")] ++ [(symbol \"2\")]))" )
             , ( "(' (, 1))"
               , "1" )
             , ( "(' (+ a (, (+ b c))))"
               , "(Sexp \"+\" ([(symbol \"a\")] ++ [(b + c)]))" )
             , ( "(' (+ 1 (,@ args) 2))"
               , "(Sexp \"+\" ([(symbol \"1\")] ++ args ++ [(symbol \"2\")]))" )
             ]

testComp name mac cases = testMacro name (parseOutput mac H.compExpr) cases

main = do 
  testComp "quote" quote quoteCases
