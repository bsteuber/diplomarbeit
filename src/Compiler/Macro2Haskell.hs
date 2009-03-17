import qualified Code as C
import qualified Control.Exception as E
import Compiler
import Haskell hiding (lines)
import qualified Model as M
import Parser
import System.Environment (getArgs)
import Util

main :: IO ()
main = do [m, hs]     <- getArgs
          putStrLn ("transforming " ++ m ++ " to " ++ hs)
          M.Text text <- M.readText m
          gen         <- return $ C.pprint 80 (C.layoutSexp (haskell (readSexps text)))
          M.writeText hs (M.Text gen)

