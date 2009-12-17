module TestComp2Haskell where
--import Parser
import Util
import Control.Arrow
import Arrows
import Model
import Sexp
import Code
import qualified Haskell as H
import Comp2Haskell
import Sexp2Haskell
import Haskell2Code


quoteCases = [ ( "(' 42)"
               , "([Symbol \"42\"])" )
             , ( "(' 1 2 3)"
               , "([Symbol \"1\"] ++ [Symbol \"2\"] ++ [Symbol \"3\"])" )
             , ( "(' (+ 1 2))"
               , "([Node ([Symbol \"+\"] ++ [Symbol \"1\"] ++ [Symbol \"2\"])])" )
             , ( "(' (, 1))"
               , "([1])" )
             , ( "(' (+ a (, (+ b c))))"
               , "([Node ([Symbol \"+\"] ++ [Symbol \"a\"] ++ [b + c])])" )
             , ( "(' (+ 1 (,@ args) 2))"
               , "([Node ([Symbol \"+\"] ++ [Symbol \"1\"] ++ args ++ [Symbol \"2\"])])" )
             ]

globalCases = [ ( "foo"
                , "foo" )
              , ( "()" 
                , "()" )
              , ( "(list me (now))"
                , "(list me (now))" )
              ]

testComp name mac cases = 
    (testCompiler 
     name 
     (((toIO mac :: IOArrow [Sexp] [Sexp]) >>>
       (compile  :: IOArrow [Sexp] H.Expr) >>>
       (compile  :: IOArrow H.Expr Code)) :: IOArrow [Sexp] Code)
     cases)

main = do testComp "quote" compQuote quoteCases
          testMacro "comp2haskell" compAuto globalCases
