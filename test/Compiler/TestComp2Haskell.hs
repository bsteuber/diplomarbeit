module TestComp2Haskell where
import Eater
import Macro
import Comp2Haskell

quoteMacro = eater2singleTrans quote
quoteCases = [ ( "(' 42)"
                , "(Symbol (str 42))" )
              , ( "(' (+ 1 2))"
                , "(Node (str +) (List (++ (List (Symbol (str 1))) (List (Symbol (str 2))))))" )
              , ( "(' (, 1))"
                , "1" )
              , ( "(' (+ a (, (+ b c))))"
                , "(Node (str +) (List (++ (List (Symbol (str a))) (List (+ b c)))))" )
              , ( "(' (+ 1 (,@ args) 2))"
                , "(Node (str +) (List (++ (List (Symbol (str 1))) args (List (Symbol (str 2))))))" )
              ]
      
main = do testMacro quoteMacro quoteCases
