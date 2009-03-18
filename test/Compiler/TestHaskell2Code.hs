module TestHaskell2Code where
import Eater
import Macro
import Haskell2Code

importMacro = eater2singleTrans eatImport
importCases = [ ( "Mod1"
                , "(words import Mod1)" )
              , ( "(Mod1)"
                , "(words import Mod1)" )
              , ( "(Mod1 (only fun))"
                , "(words import Mod1 (parens fun))" )
              , ( "(Mod1 (only fun1 fun2))"
                , "(words import Mod1 (parens (commaSep fun1 fun2)))" )
              , ( "(Mod1 (hiding fun))"
                , "[import Mod1 hiding (fun)]" ) 
              , ( "(Mod1 (hiding fun1 fun2 fun3))"
                , "[import Mod1 hiding (fun1, fun2, fun3)]" )
              , ( "(Mod1 (qualified M))"
                , "[import qualified Mod1 as M]" )
              ]

moduleMacro = eater2singleTrans eatModule
moduleCases = [( "(module MyMod)"
               , "[module MyMod where]" )
              , ( "(module MyMod Control.Exception (Prelude (hiding words)))"
                , "[module MyMod where\nimport Control.Exception\nimport Prelude hiding (words)]" )
              ]

typedMacro = eater2singleTrans eatTyped
typedCases = [ ( "Int"
              , "[Int]" )
            , ( "a"
              , "[a]" )
            , ( "(Fun a b)"
              , "[(a -> b)]" )
            , ( "(Fun a b c d)"
              , "[(a -> b -> c -> d)]" )
            , ( "(List Int)"
              , "[[Int]]" )
            , ( "(Tuple)"
              , "[()]" )
            , ( "(Tuple a b)"
              , "[(a, b)]" )
            , ( "(Tuple a b (Fun a b))"
              , "[(a, b, (a -> b))]" )
            ]

typeMacro = eater2singleTrans eatType
typeCases = [ ( "(type x Int)"
              , "[x :: Int]" )
            , ( "(type myMap (Fun (Fun a b) (List a) (List b)))" 
              , "[myMap :: ((a -> b) -> [a] -> [b])]" )
            , ( "(type x Sexp)" 
              , "[x :: Sexp]" )
            , ( "(type x (Failing Int))" 
              , "[x :: (Failing Int)]" )
            , ( "(type (f x y) Unit)" 
              , "[(f x y) :: ()]" )
            ]

defMacro = eater2singleTrans eatDef
defCases = [ ( "(= x 5)"
             , "[x = 5]" )
           , ( "(= (x) 42)"
             , "[x = 42]" )
           , ( "(= (f _ _) 33)"
             , "[f _ _ = 33]" )
           , ( "(= (|> x y) (y x))"
             , "[(|>) x y = (y x)]" )
           , ( "(= con (str 12))"
             , "[con = \"12\"]" )
           , ( "(= con (List (str 12)))"
             , "[con = [\"12\"]]" )
           , ( "(= con (List (str 12) (str 32)))"
             , "[con = [\"12\", \"32\"]]" )
           , ( "(= res (f 1 2) (where (= (f a b) (+ a b))))"
             , "[res = (f 1 2)\n  where\n    f a b = (a + b)]" )
           ]


exprMacro = eater2singleTrans eatExpr
exprCases = [ ( "(str 42)"
                , "\"42\"" )
              , ( "(str a  b c)"
                , "\"abc\"" )
              , ( "(List a b (str c))"
                , "[[a, b, \"c\"]]" )
              , ( "(map + myList)"
                , "(map (+) myList)" )
              ]

lambdaMacro = eater2singleTrans eatLambda
lambdaCases = [ ( "(fun x 42)"
                , "[(\\ x -> 42)]" )
              , ( "(fun (args x) 42)"
                , "[(\\ x -> 42)]" )
              , ( "(fun (args x (List y)) (plus x y))"
                , "[(\\ x [y] -> (plus x y))]" )
              ]

doMacro = eater2singleTrans eatDo
doCases = [ ( "(do 42)"
            , "[(do\n  42)]" )
          , ( "(do (<- x 42) b)"
            , "[(do\n  x <- 42\n  b)]" )
          , ( "(do (<- x 42) (<- y (+ a b)) (return x))"
            , "[(do\n  x <- 42\n  y <- (a + b)\n  (return x))]" )
          ]
      
main = do testMacro importMacro importCases
          testMacro moduleMacro moduleCases
          testMacro  typedMacro  typedCases
          testMacro   typeMacro   typeCases
          testMacro    defMacro    defCases
          testMacro   exprMacro   exprCases
          testMacro lambdaMacro lambdaCases
          testMacro     doMacro     doCases
