module Main where

import Prelude hiding (catch)
import System.IO
import Control.Monad
import Control.Exception
import Util
import Reader
import Macro
import Code2String
import Haskell2Code

firstPos :: Char -> String -> Maybe Int
firstPos _ []     = Nothing
firstPos elt (x:xs)  = 
    if elt == x
       then Just 0
       else liftM (1+) (firstPos x xs)

split :: Char -> String -> [String]
split char str =
    case firstPos char str of
      Nothing -> [str]
      Just p  -> take (p+1) str : split char (drop (p+1) str)

printLines :: [String] -> IO ()
printLines [] = return ()
printLines (str:strs) = 
    do putStrLn str
       printLines strs

repl :: IO ()
repl = do putStr "\n>"
          hFlush stdout
          input <- getLine
          if input == ":q"
            then 
                return ()
            else
                do catch (trans input) catchFun
                   repl     
    where trans = putStrLn . 
                  code2string . 
                  trans2fun CompileException haskell2code . 
                  readSexp
          catchFun :: MagiclException -> IO ()
          catchFun e = e |> show |> putStrLn

main :: IO ()
main = repl