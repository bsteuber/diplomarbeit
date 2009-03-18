module Sexp where
import Control.Monad
import Util

data Sexp = Symbol {symbolName   :: String}
          | Node   {nodeName     :: String,
                    nodeChildren :: Stream}
    deriving Eq

type Stream = [Sexp]

type SymbolTrans a = String -> Failing a
type SymbolPred = String -> Bool

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
