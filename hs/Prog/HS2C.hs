module HS2C where
import Control.Arrow
import System.IO
import Arrows
import Parser
import Model
import Sexp
import Haskell
import Code
import Sexp2Haskell
import Haskell2Code

main = compiler ((compile :: IOArrow [Sexp] Haskell) >>> 
--                 (skip (Kleisli $ const $ hPutStrLn stderr "half")) >>>
                 (compile :: IOArrow Haskell Code))