module Haskell2Code where
import Prelude hiding (lines, words)
import Control.Arrow
import Arrows
import Sexp
import Parser
import Compiler

text :: String -> Sexp
text = singleNode "text" . symbol

newline :: Sexp
newline = symbol "newline"

parens :: Sexp -> Sexp
parens = singleNode "parens"

brackets :: Sexp -> Sexp
brackets = singleNode "brackets"

lines = node "lines"
append = node "append"
paragraphs = node "paragraphs"
words = node "words"
textWords = map text >>> words
commaSep = node "commaSep"
noFlat = singleNode "noFlat"
indent x = node "indent" [symbol "2", x]

foldOp :: String -> [Sexp] -> Sexp
foldOp op stream = node "foldOp" (symbol op : stream)

parenFoldOp op = parens . foldOp op

binOp op x y = foldOp op [x, y]
binParenOp op x y = parens $ binOp op x y

tuple = parens . commaSep
list = brackets . commaSep

compModule :: SexpParser a Sexp
compModule = 
    macro "module" (consArrow
                    (takeSymbol >>> arr (\s -> textWords ["module", s, "where"]))
                    (many compImport)
                   ) >>> arr lines

compImport :: SexpParser a Sexp
compImport = 
    takeAnySexp $ (arr snd &&&
                 (impNormal <+> impQualified <+> impHiding <+> impOnly)) >>> arr imp
    where impNormal              = empty >>> constArrow ([], [])
          impQualified           = macro "qualified" (takeSymbol >>> arr calcQualified)
          calcQualified short    = ([text "qualified"], [text "as", text short])
          impHiding              = macro "hiding" (many takeSymbol >>> arr calcHiding)
          calcHiding hides       = ([], [text "hiding", tuple (map text hides)])
          impOnly                = macro "only" (many takeSymbol >>> arr calcOnly)
          calcOnly onlys         = ([], [tuple (map text onlys)])
          imp (name, (bef, aft)) = words $ [text "import"] ++ bef ++ [text name] ++ aft

-- compTyped :: SexpParser a Sexp
-- compTyped = compOrs [ compSymbolNamed "Unit"  (text "()")
--                   , compNodeNamed   "List"  (liftM brackets compTyped)
--                   , compNodeNamed   "Tuple" (liftM (tuple) (manySexp compTyped))
--                   , compSymbolFun text
--                   , compNodeNamed "Fun"   (liftM (parenFoldOp "->")  (manySexp compTyped))
--                   , compNodeFun $ \genType -> do
--                       args <- manySexp compTyped
--                       return $ parens $ words $ text genType : args
--                  ]

-- isOp :: String -> Bool
-- isOp = all (`elem` "!$%&/=?*+-.:<|>")

-- compCallWithoutParens = 
--     (compOr 
--      (compSymbolFun text)
--      (compNodeFun $ \lbl -> do
--         let prefixFun = if isOp lbl then "("++lbl++")" else lbl
--         tokens <- manySexp compExpr
--         return $ words $ text prefixFun : tokens))

-- compCall :: SexpParser a Sexp
-- compCall = (compOr 
--            (compSymbolFun text)
--            (compNodeFun $ \lbl -> do
--               tokens <- manySexp compExpr
--               return $ if isOp lbl then
--                            (parenFoldOp lbl tokens)
--                        else
--                            (node lbl tokens)))

-- compLhs = compOrs [ compSymbolNamed "Unit"  $ text "()"
--                 , compNodeNamed   "List"  $ liftM list $ manySexp compExpr
--                 , compNodeNamed   "Tuple" $ liftM tuple $ manySexp compExpr
--                 , compCall
--                 ]

-- compType :: SexpParser a Sexp
-- compType = compNodeNamed "type" (liftM2 (binOp "::") (compLhs) (compTyped))

-- compString =
--     compNodeNamed "str" (do exps <- manySexp $ compSymbolFun text
--                            return $ append $ strQ ++ exps ++ strQ)
--         where strQ = [text "\""]


-- compLambda = compNodeNamed "fun" (do 
--                                  args <- (compOr 
--                                          (compNodeNamed "args" (manySexp compLhs)))
--                                          (liftM single compLhs)
--                                  body <- compExpr
--                                  return $ node "\\" $ args ++ [text "->", body]
--                                )                                                                           

-- compDo = compNodeNamed "do" (do
--                             cmds <- manySexp (compOr
--                                             (compNodeNamed "<-" (liftM2 (binOp "<-")
--                                                                        compLhs
--                                                                        compExpr))
--                                             compExpr)
--                             return $ noFlat $ node "do" cmds)
                            

-- compExpr :: SexpParser a Sexp
-- compExpr = compOrs [ compString
--                  , compLambda
--                  , compDo
--                  , compType
--                  , compSymbolSatisfying isOp (parens . text)
--                  , compLhs
--                  ]

-- compDef :: SexpParser a Sexp
-- compDef = compNodeNamed "=" (do
--                             res <- (liftM2 (binOp "=") compCallWithoutParens compExpr)
--                             whereDefs <- liftM concat $ compMaybe $ compNodeNamed "where" $ manySexp compTopLevel
--                             return $ if null whereDefs
--                                      then
--                                          res
--                                      else
--                                          append [res, indent (append [newline, indent $ lines $ text "where" : whereDefs])])

-- compTopLevel = compOr compType compDef

-- haskell2code :: Macro
-- haskell2code = comper2singleTrans $
--                compNodeNamed "haskell" (liftM2
--                                        (\a b -> paragraphs (a ++ b)) 
--                                        (compMaybe compModule)
--                                        (manySexp compTopLevel))
          