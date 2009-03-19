module Haskell2Code where --(haskell, haskellStream) where
import Prelude hiding (lines, words)
import Data.Char
import Control.Monad
import System.Environment (getArgs)
import Util
import Sexp
import Eater
import Reader
import Code2String

import qualified Control.Exception as E

-- Utility

text :: String -> Sexp
text = singleNode "text" . Symbol

newline :: Sexp
newline = Symbol "newline"

parens :: Sexp -> Sexp
parens = singleNode "parens"

brackets :: Sexp -> Sexp
brackets = singleNode "brackets"

lines = node "lines"
append = node "append"
paragraphs = node "paragraphs"
words = node "words"
commaSep = node "commaSep"
noFlat = singleNode "noFlat"
indent x = node "indent" [Symbol "2", x]

foldOp :: String -> [Sexp] -> Sexp
foldOp op stream = node "foldOp" (Symbol op : stream)

parenFoldOp op = parens . foldOp op

binOp op x y = foldOp op [x, y]
binParenOp op x y = parens $ binOp op x y

tuple = parens . commaSep
list = brackets . commaSep

eatImpArgs :: SexpEater ([Sexp], [Sexp])
eatImpArgs = eatOrs [ (do eatEmpty; return ([], [])),
                      (do mods <- eatNodeNamed "hiding" $ manySexp $ eatSymbolFun text
                          return ([], [text "hiding", tuple mods])),
                      (do mods <- eatNodeNamed "only" $ manySexp $ eatSymbolFun text
                          return ([], [tuple mods])),
                      (do short <- eatSymbolFun text |> eatNodeNamed "qualified"
                          return ([text "qualified"], [text "as", short]))
                    ]


eatImport :: SexpEater Sexp
eatImport = do (imp, bef, aft) <- (eatOr 
                                  (eatNodeFun (\imp -> do 
                                                 (bef,aft) <- eatImpArgs
                                                 return (imp, bef, aft)))
                                  (eatSymbolFun (\s -> (s, [], []))))
               ([text "import"] ++ bef ++ [text imp] ++ aft) |> words |> return

eatModule :: SexpEater Sexp
eatModule = 
    (eatNodeNamed
     "module"
     (do modDef <- eatSymbolFun (\s -> ["module", s, "where"] |> map text |> words)
         imports <- eatImport |> manySexp
         (modDef:imports) |> lines |> return))



eatTyped :: SexpEater Sexp
eatTyped = eatOrs [ eatSymbolNamed "Unit"  (text "()")
                  , eatNodeNamed   "List"  (liftM brackets eatTyped)
                  , eatNodeNamed   "Tuple" (liftM (tuple) (manySexp eatTyped))
                  , eatSymbolFun text
                  , eatNodeNamed "Fun"   (liftM (parenFoldOp "->")  (manySexp eatTyped))
                  , eatNodeFun $ \genType -> do
                      args <- manySexp eatTyped
                      return $ parens $ words $ text genType : args
                 ]

isOp :: String -> Bool
isOp = all (`elem` "!$%&/=?*+-.:<|>")

eatCallWithoutParens = 
    (eatOr 
     (eatSymbolFun text)
     (eatNodeFun $ \lbl -> do
        let prefixFun = if isOp lbl then "("++lbl++")" else lbl
        tokens <- manySexp eatExpr
        return $ words $ text prefixFun : tokens))

eatCall :: SexpEater Sexp
eatCall = (eatOr 
           (eatSymbolFun text)
           (eatNodeFun $ \lbl -> do
              tokens <- manySexp eatExpr
              return $ if isOp lbl then
                           (parenFoldOp lbl tokens)
                       else
                           (node lbl tokens)))

eatLhs = eatOrs [ eatSymbolNamed "Unit"  $ text "()"
                , eatNodeNamed   "List"  $ liftM list $ manySexp eatExpr
                , eatNodeNamed   "Tuple" $ liftM tuple $ manySexp eatExpr
                , eatCall
                ]

eatType :: SexpEater Sexp
eatType = eatNodeNamed "type" (liftM2 (binOp "::") (eatLhs) (eatTyped))

eatString =
    eatNodeNamed "str" (do exps <- manySexp $ eatSymbolFun text
                           return $ append $ strQ ++ exps ++ strQ)
        where strQ = [text "\""]


eatLambda = eatNodeNamed "fun" (do 
                                 args <- (eatOr 
                                         (eatNodeNamed "args" (manySexp eatLhs)))
                                         (liftM single eatLhs)
                                 body <- eatExpr
                                 return $ node "\\" $ args ++ [text "->", body]
                               )                                                                           

eatDo = eatNodeNamed "do" (do
                            cmds <- manySexp (eatOr
                                            (eatNodeNamed "<-" (liftM2 (binOp "<-")
                                                                       eatLhs
                                                                       eatExpr))
                                            eatExpr)
                            return $ noFlat $ node "do" cmds)
                            

eatExpr :: SexpEater Sexp
eatExpr = eatOrs [ eatString
                 , eatLambda
                 , eatDo
                 , eatType
                 , eatSymbolSatisfying isOp (parens . text)
                 , eatLhs
                 ]

eatDef :: SexpEater Sexp
eatDef = eatNodeNamed "=" (do
                            res <- (liftM2 (binOp "=") eatCallWithoutParens eatExpr)
                            whereDefs <- liftM concat $ eatMaybe $ eatNodeNamed "where" $ manySexp eatTopLevel
                            return $ if null whereDefs
                                     then
                                         res
                                     else
                                         append [res, indent (append [newline, indent $ lines $ text "where" : whereDefs])])

eatTopLevel = eatOr eatType eatDef

haskell2code :: Macro
haskell2code = eater2singleTrans $
               eatNodeNamed "haskell" (liftM2
                                       (\a b -> paragraphs (a ++ b)) 
                                       (eatMaybe eatModule)
                                       (manySexp eatTopLevel))
          