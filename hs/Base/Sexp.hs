module Sexp where
import Control.Monad
import qualified Control.Exception as E
import Util
import Eater
import Code2String

symbol = Symbol
node   = Node

singleNode :: String -> Sexp -> Sexp
singleNode str sexp = Node str [sexp]

symbolNameEq name = (name==) . symbolName
nodeNameEq name = (name==) . nodeName

quoteSymbol :: String -> Sexp
quoteSymbol symName = 
    Node "Symbol" [Node "str" [Symbol symName]]

quoteNode :: String -> Stream -> Sexp
quoteNode nodName nodChildren =
    Node "Node" [Node "str" [Symbol nodName],
                 Node "List" nodChildren]

eatError :: String -> SexpEater a
eatError msg = Eater $ \ stream -> E.throw $ CompileException $ msg ++ ":\n" ++ show stream
                
eatSymbol :: SymbolTrans a -> SexpEater a
eatSymbol trans = 
    trans2eater (\ sexp -> 
                     case sexp of
                       Node _ _ -> Error "eatSymbol: node given"
                       Symbol s -> trans s)

eatSymbolFun = eatSymbol . fun2trans
eatAnySymbol = eatSymbolFun id

eatNode :: SymbolTrans (SexpEater a) -> SexpEater a
eatNode trans = 
    trans2eater (\ sexp -> 
                     case sexp of
                       Node name stream -> do
                                 eater <- trans name
                                 eater2trans eater stream
                       Symbol _  -> Error "eatNode: symbol given")

eatNodeFun = eatNode . fun2trans
eatAnyNode eater = eatNode $ const $ Success eater

eatSymbolSatisfying :: SymbolPred -> (String -> a) -> SexpEater a
eatSymbolSatisfying p trans =
    eatSymbol (\ sym -> if p sym
                       then 
                           Success $ trans sym
                       else
                           Error "eatSymbolSatisfying: predicate not true")

eatNodeSatisfying :: SymbolPred -> (String -> (SexpEater a)) -> SexpEater a
eatNodeSatisfying p trans =
    eatNode (\ sym -> if p sym
                     then 
                         Success $ eatOr (trans sym) (eatError "innner node eater failed")
                     else
                         Error "eatNodeSatisfying: predicate not true")

eatSymbolNamed str res = eatSymbolSatisfying (== str) (const res)
eatNodeNamed str eater = eatNodeSatisfying (== str) (const eater)

sexpSplice :: Stream -> Stream
sexpSplice = concat . map f
    where f (Node "returnAll" stream) = stream
          f x = [x]

manySexp :: SexpEater Sexp -> SexpEater Stream
manySexp = liftM sexpSplice . eatAll
