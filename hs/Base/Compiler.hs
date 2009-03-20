module Compiler where
import Control.Arrow
import Util
import Arrows
import Sexp
import Parser
import Reader
import Writer

type SexpParser = Parser Sexp

sexpNamed :: String -> SexpParser a [Sexp]
sexpNamed name = 
    (when
     (labelEq name) 
     (\ Sexp lbl _ -> "Expected symbol " ++ name ++ "\nGot " ++ lbl))

macro :: String -> SexpParser a b -> SexpParser a b
macro name parseInner =
    (sexpNamed name &&& id) >>> (id &&& get

-- eatNodeSatisfying :: SymbolPred -> (String -> (SexpEater a)) -> SexpEater a

-- sexpSplice :: Stream -> Stream
-- sexpSplice = concat . map f
--     where f (Node "returnAll" stream) = stream
--           f x = [x]

-- manySexp :: SexpEater Sexp -> SexpEater Stream
-- manySexp = liftM sexpSplice . eatAll


compile :: SexpParser () a -> Sexp -> IO a
compile p sexp = execParser p [sexp]

compileStr :: (Show a) => SexpParser () a -> String -> IO String
compileStr p str = do
  sexp <- readSexp str
  res <- compile p sexp
  return $ show res

-- testMacro :: Macro -> [(String, String)] -> IO ()
-- testMacro _ [] = do putStrLn "Hooray! Tests passed."
--                     return ()
-- testMacro macro ((src, tgt):cases) = 
--     (E.catch 
--      (let res = compileStr macro src
--           exp = (code2string $ readSexp tgt)
--       in 
--         if res == exp
--         then
--             testMacro macro cases
--         else
--             ("Test " ++ (show $ readSexp src) ++ ":\n  Expected:\n" ++ exp ++ "\n  Got:\n" ++ res) |> TestException |> E.throw)
--      catchFun)
--     where catchFun :: MagiclException -> IO ()
--           catchFun e = ("Error when compiling test case:\n" ++ show src ++ "\n" ++ show e) |> putStrLn

                
