module Model where
import System.IO
import Control.Monad
import Sexp
import Code2String
import Reader
    
class Model a where
    modelToSexp :: Compiler a Sexp
    sexpToModel :: Compiler Sexp a

type Compiler a b = a -> b
type Writer a = a -> IO ()
type Reader a = IO a

instance Model Sexp where
    modelToSexp s = s
    sexpToModel s = s

data Text = Text String

instance Model Text where
    modelToSexp = textToSexp
    sexpToModel = sexpToText

textToSexp :: Compiler Text Sexp
textToSexp (Text str) = readSexp str

sexpToText :: Compiler Sexp Text
sexpToText sexp = Text (show sexp)

modelToText :: (Model a) => Compiler a Text
modelToText = sexpToText . modelToSexp

textToModel :: (Model a) => Compiler Text a
textToModel = sexpToModel . textToSexp

writeText :: FilePath -> Writer Text
writeText path (Text str) = writeFile path str

readText :: FilePath -> Reader Text
readText path = liftM Text (readFile path)

writeModel :: (Model a) => FilePath -> Writer a
writeModel path = (writeText path) . modelToText

readModel :: (Model a) => FilePath -> Reader a
readModel path = liftM textToModel (readText path)
