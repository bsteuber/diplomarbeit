module TestHaskell2Code where
import Control.Arrow
import Arrows
import Parser
import Compiler
import Haskell
import Sexp2Haskell
import Haskell2Code


importCases = [ ( "Mod1"
                , "import Mod1" )
              , ( "(Mod1)"
                , "import Mod1" )
              , ( "(Mod1 (only fun))"
                , "import Mod1 (fun)" )
              , ( "(Mod1 (only fun1 fun2))"
                , "import Mod1 (fun1, fun2)" )
              , ( "(Mod1 (hiding fun))"
                , "import Mod1 hiding (fun)" ) 
              , ( "(Mod1 (hiding fun1 fun2 fun3))"
                , "import Mod1 hiding (fun1, fun2, fun3)" )
              , ( "(Mod1 (qualified M))"
                , "import qualified Mod1 as M" )
              ]

moduleCases = [ ( "(module MyMod)"
               , "module MyMod where" )
              , ( "(module MyMod Control.Exception (Prelude (hiding words)))"
                , "module MyMod where\nimport Control.Exception\nimport Prelude hiding (words)" )
              ]

typeCases = [ ( "Int"
              , "Int" )
            , ( "a"
              , "a" )
            , ( "(Fun a b)"
              , "(a -> b)" )
            , ( "(Fun a b c d)"
              , "(a -> b -> c -> d)" )
            , ( "(List Int)"
              , "[Int]" )
            , ( "(Tuple)"
              , "()" )
            , ( "(Tuple a b)"
              , "(a, b)" )
            , ( "(Tuple a b (Fun a b))"
              , "(a, b, (a -> b))" )
            ]

typeDefCases = [ ( "(type x Int)"
              , "x :: Int" )
            , ( "(type myMap (Fun (Fun a b) (List a) (List b)))" 
              , "myMap :: ((a -> b) -> [a] -> [b])" )
            , ( "(type x Sexp)" 
              , "x :: Sexp" )
            , ( "(type x (Failing Int))" 
              , "x :: (Failing Int)" )
            , ( "(type (f x y) Unit)" 
              , "(f x y) :: ()" )
            ]

defCases = [ ( "(= x 5)"
             , "x = 5" )
           , ( "(= (x) 42)"
             , "x = 42" )
           , ( "(= (f _ _) 33)"
             , "f _ _ = 33" )
           , ( "(= (|> x y) (y x))"
             , "x |> y = (y x)" )
           , ( "(= con (Str 12))"
             , "con = \"12\"" )
           , ( "(= con (List (Str 12)))"
             , "con = [\"12\"]" )
           , ( "(= con (List (Str 12) (Str 32)))"
             , "con = [\"12\", \"32\"]" )
           , ( "(= res (f 1 2) (where (= (f a b) (+ a b))))"
             , "res = (f 1 2)\n  where\n    f a b = (a + b)" )
           ]


exprCases = [ ( "(Str 42)"
                , "\"42\"" )
              , ( "(Str a  b c)"
                , "\"a b c\"" )
              , ( "(List a b (Str c))"
                , "[a, b, \"c\"]" )
              , ( "(map + myList)"
                , "(map (+) myList)" )
              ]

lambdaCases = [ ( "(fun x 42)"
                , "(\\ x -> 42)" )
              , ( "(fun (args x) 42)"
                , "(\\ x -> 42)" )
              , ( "(fun (args x (List y)) (plus x y))"
                , "(\\ x [y] -> (plus x y))" )
              , ( "(fun (Node n) (show n))"
                , "(\\ (Node n) -> (show n))" )
              ]

doCases = [ ( "(do 42)"
            , "(do\n  42)" )
          , ( "(do (<- x 42) b)"
            , "(do\n  x <- 42\n  b)" )
          , ( "(do (<- x 42) (<- y (+ a b)) (return x))"
            , "(do\n  x <- 42\n  y <- (a + b)\n  (return x))" )
          ]

main = do testMacro "import"  (comp :: SexpParser () Import)   importCases
          testMacro "module"  (comp :: SexpParser () Module)   moduleCases
          testMacro "type"    (comp :: SexpParser () Type)       typeCases
          testMacro "typedef" (comp :: SexpParser () TypeDef) typeDefCases
          testMacro "def"     (comp :: SexpParser () Def)         defCases
          testMacro "expr"    (comp :: SexpParser () Expr)       exprCases
          testMacro "lambda"  (comp :: SexpParser () Expr)     lambdaCases
          testMacro "do"      (comp :: SexpParser () Expr)         doCases

