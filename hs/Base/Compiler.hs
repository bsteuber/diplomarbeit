module Compiler where
import Util
import Sexp
import Parser
import Reader
import Writer

type Macro = Parser Sexp () Sexp

compile :: Macro -> Sexp -> IO Sexp
compile macro sexp = execParser macro [sexp]

compileStr :: Macro -> String -> IO String
compileStr macro str = do
  sexp <- readSexp str
  res <- compile macro sexp
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

-- eatError :: String -> SexpEater a
-- eatError msg = Eater $ \ stream -> E.throw $ CompileException $ msg ++ ":\n" ++ show stream
                
-- eatSymbol :: SymbolTrans a -> SexpEater a
-- eatSymbol trans = 
--     trans2eater (\ sexp -> 
--                      case sexp of
--                        Node _ _ -> Error "eatSymbol: node given"
--                        Symbol s -> trans s)

-- eatSymbolFun = eatSymbol . fun2trans
-- eatAnySymbol = eatSymbolFun id

-- eatNode :: SymbolTrans (SexpEater a) -> SexpEater a
-- eatNode trans = 
--     trans2eater (\ sexp -> 
--                      case sexp of
--                        Node name stream -> do
--                                  eater <- trans name
--                                  eater2trans eater stream
--                        Symbol _  -> Error "eatNode: symbol given")

-- eatNodeFun = eatNode . fun2trans
-- eatAnyNode eater = eatNode $ const $ Success eater

-- eatSymbolSatisfying :: SymbolPred -> (String -> a) -> SexpEater a
-- eatSymbolSatisfying p trans =
--     eatSymbol (\ sym -> if p sym
--                        then 
--                            Success $ trans sym
--                        else
--                            Error "eatSymbolSatisfying: predicate not true")

-- eatNodeSatisfying :: SymbolPred -> (String -> (SexpEater a)) -> SexpEater a
-- eatNodeSatisfying p trans =
--     eatNode (\ sym -> if p sym
--                      then 
--                          Success $ eatOr (trans sym) (eatError "innner node eater failed")
--                      else
--                          Error "eatNodeSatisfying: predicate not true")

-- eatSymbolNamed str res = eatSymbolSatisfying (== str) (const res)
-- eatNodeNamed str eater = eatNodeSatisfying (== str) (const eater)

-- sexpSplice :: Stream -> Stream
-- sexpSplice = concat . map f
--     where f (Node "returnAll" stream) = stream
--           f x = [x]

-- manySexp :: SexpEater Sexp -> SexpEater Stream
-- manySexp = liftM sexpSplice . eatAll
