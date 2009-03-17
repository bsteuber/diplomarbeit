module Haskell2Code where --(haskell, haskellStream) where
import Prelude hiding (lines, words)
import Data.Char
import Control.Monad
import System.Environment (getArgs)
import Util
import Sexp
import Eater
import Reader
--import Model
import Macro
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
                      (do mods <- eatNodeNamed "hiding" $ eatMany $ eatSymbolFun text
                          return ([], [text "hiding", tuple mods])),
                      (do mods <- eatNodeNamed "only" $ eatMany $ eatSymbolFun text
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
         imports <- eatImport |> eatMany
         (modDef:imports) |> lines |> return))



eatTyped :: SexpEater Sexp
eatTyped = eatOrs [ eatSymbolNamed "Unit"  (text "()")
                  , eatNodeNamed   "List"  (liftM brackets eatTyped)
                  , eatNodeNamed   "Tuple" (liftM (tuple) (eatMany eatTyped))
                  , eatSymbolFun text
                  , eatNodeNamed "Fun"   (liftM (parenFoldOp "->")  (eatMany eatTyped))
                  , eatNodeFun $ \genType -> do
                      args <- eatMany eatTyped
                      return $ parens $ words $ text genType : args
                 ]

isOp :: String -> Bool
isOp = all (`elem` "!$%&/=?*+-.<|>")

eatCallWithoutParens = 
    (eatOr 
     (eatSymbolFun text)
     (eatNodeFun $ \lbl -> do
        let prefixFun = if isOp lbl then "("++lbl++")" else lbl
        tokens <- eatMany eatExpr
        return $ words $ text prefixFun : tokens))

eatCall :: SexpEater Sexp
eatCall = (eatOr 
           (eatSymbolFun text)
           (eatNodeFun $ \lbl -> do
              tokens <- eatMany eatExpr
              return $ if isOp lbl then
                           (parenFoldOp lbl tokens)
                       else
                           (node lbl tokens)))

eatLhs = eatOrs [ eatSymbolNamed "Unit"  $ text "()"
                , eatNodeNamed   "List"  $ liftM list $ eatMany eatExpr
                , eatNodeNamed   "Tuple" $ liftM tuple $ eatMany eatExpr
                , eatCall
                ]

eatType :: SexpEater Sexp
eatType = eatNodeNamed "type" (liftM2 (binParenOp "::") (eatLhs) (eatTyped))

eatString =
    eatNodeNamed "str" (do exps <- eatMany $ eatExpr
                           return $ append $ strQ ++ exps ++ strQ)
        where strQ = [text "\""]


eatLambda = eatNodeNamed "fun" (do 
                                 args <- (eatOr 
                                         (eatNodeNamed "args" (eatMany eatLhs)))
                                         (liftM single eatLhs)
                                 body <- eatExpr
                                 return $ node "\\" $ args ++ [text "->", body]
                               )                                                                           

eatDo = eatNodeNamed "do" (do
                            cmds <- eatMany (eatOr
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
                 , eatLhs
                 ]

eatDef :: SexpEater Sexp
eatDef = eatNodeNamed "=" (do
                            res <- (liftM2 (binOp "=") eatCallWithoutParens eatExpr)
                            whereDefs <- liftM concat $ eatMaybe $ eatNodeNamed "where" $ eatMany eatDef
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
                                       (eatMany eatTopLevel))
          