module FileCompiler (fileCompiler) where
import System.Environment (getArgs)
import System.Directory
import Sexp

fileCompiler :: Macro -> IO ()
fileCompiler macro = do
  [inFile, outFile] <- getArgs
  input <- readFile inFile
  let output = compileStr macro input
  writeFile outFile output
