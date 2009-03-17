import Data.Char
import Data.Maybe
import Control.Monad
import System
import System.Environment (getArgs)
import System.Directory
import System.FilePath (takeBaseName)

whenNewer :: FilePath -> FilePath -> IO ExitCode -> IO ExitCode
whenNewer inFile outFile action =
    do exist  <- doesFileExist outFile
       if exist
          then do
            inMod  <- getModificationTime inFile
            outMod <- getModificationTime outFile
            if inMod > outMod then action else return ExitSuccess
          else action

sysList = system . unwords

ghcLoadPath = "-isrc/Base:src/Compiler:gen/Compiler"

ghcCall = "ghc --make " ++ ghcLoadPath ++ " -outputdir gen/ghc"

ghc file =
    (whenNewer 
     inFile
     outFile 
     (sysList [ghcCall, "-o", outFile, "-main-is", modName, inFile]))
  where modName = takeBaseName file
        outFile = "gen/bin/" ++ map toLower modName
        inFile  = file ++ ".hs"
        
ghci = sysList ["rlwrap", "ghci", ghcLoadPath] 

runCompiler comp inFile outFile =
    (whenNewer
     inFile 
     outFile
     (sysList ["gen/bin/" ++ comp, inFile, outFile] ))

hs2c = runCompiler "hs2c"

runBin = system . ("gen/bin/"++)

testCompiler comp = do
  ghc    $ "test/Compiler/Test" ++ comp
  runBin $ "test" ++ map toLower comp

testCompilers = mapM testCompiler

test = testCompilers ["Haskell2Code", "Comp2Haskell"]

mkDirs = mapM $ \ dir -> system $ "mkdir -p " ++ dir

build = do
  mkDirs ["gen/bin", "gen/ghc", "gen/Compiler"]
  ghc "src/Progs/HS2C"
  hs2c "src/Compiler/Comp2Haskell.sep" "gen/Compiler/Comp2Haskell.hs"
  ghc "src/Progs/CMP2HS"

main = do args  <- getArgs
          case args of
            ["ghci"]                -> ghci
            ["test"]                -> test >> return ExitSuccess
--            "repl" : _              -> system "rlwrap gen/bin/repl"
            [comp, inFile, outFile] -> runCompiler comp inFile outFile
            [] -> build
