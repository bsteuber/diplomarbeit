module TestHaskell2Code where
import Control.Arrow
import Arrows
import Parser
import Sexp
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
            , ( "(type (f x y) (dep (Show a) (Eq a)) (List a))" 
              , "(f x y) :: ((Show a), (Eq a)) => [a]" )
            ]

dataCases = [ ( "(data Blub (Blub String))"
              , "data Blub =\n  Blub String" )
            , ( "(data (StrangeEither a b) (Left a) (Right b) (Nada))"
              , "data (StrangeEither a b) =\n  Left a |\n  Right b |\n  Nada" )
            ]

classCases = [ ( "(class (X a) (where (type bla Unit)))"
               , "class (X a)\n  where\n    bla :: ()" )
             , ( "(class (dep (Show a) (Eq b)) (MyClass a b) (where (type f (Fun a b)) (type g (Fun b a))))"
               , "class ((Show a), (Eq b)) => (MyClass a b)\n  where\n    f :: (a -> b)\n    g :: (b -> a)" )
             ]

instanceCases = [ ( "(instance (X Y) (where (= bla blub)))"
                  , "instance (X Y)\n  where\n    bla = blub" )
                , ( "(instance (A (Fun B c) D) (where (= (f x) (g x))))"
                  , "instance (A (B -> c) D)\n  where\n    f x = (g x)" )
                , ( "(instance (dep (Blub b)) (A (Fun b c)) (where (= (f x) (g x))))"
                  , "instance ((Blub b)) => (A (b -> c))\n  where\n    f x = (g x)" )
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

main = do testMacro "import"   (comp :: SexpParser Import)     importCases
          testMacro "module"   (comp :: SexpParser Module)     moduleCases
          testMacro "type"     (comp :: SexpParser Type)         typeCases
          testMacro "typedef"  (comp :: SexpParser Toplevel)  typeDefCases
          testMacro "data"     (comp :: SexpParser Toplevel)     dataCases
          testMacro "class"    (comp :: SexpParser Toplevel)    classCases
          testMacro "instance" (comp :: SexpParser Toplevel) instanceCases
          testMacro "def"      (comp :: SexpParser Toplevel)      defCases
          testMacro "expr"     (comp :: SexpParser Expr)         exprCases
          testMacro "lambda"   (comp :: SexpParser Expr)       lambdaCases
          testMacro "do"       (comp :: SexpParser Expr)           doCases

