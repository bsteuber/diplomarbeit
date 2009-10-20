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
               , "([symbol \"42\"])" )
             , ( "(' 1 2 3)"
               , "([symbol \"1\"] ++ [symbol \"2\"] ++ [symbol \"3\"])" )
             , ( "(' (+ 1 2))"
               , "([node ([symbol \"+\"] ++ [symbol \"1\"] ++ [symbol \"2\"])])" )
             , ( "(' (, 1))"
               , "([1])" )
             , ( "(' (+ a (, (+ b c))))"
               , "([node ([symbol \"+\"] ++ [symbol \"a\"] ++ [b + c])])" )
             , ( "(' (+ 1 (,@ args) 2))"
               , "([node ([symbol \"+\"] ++ [symbol \"1\"] ++ args ++ [symbol \"2\"])])" )
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
          testMacro "comp2haskell" comp2haskell globalCases
