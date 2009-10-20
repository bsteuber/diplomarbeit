import Data.Char
import Data.Maybe
import Control.Monad
import System
import System.Environment (getArgs)
import System.Directory
import System.FilePath (takeBaseName, (</>))

debug = False --True

whenNewer :: FilePath -> FilePath -> IO ExitCode -> IO ExitCode
whenNewer inFile outFile action =
    do exist <- doesFileExist outFile
       (if exist
        then
            (do inMod  <- getModificationTime inFile
                outMod <- getModificationTime outFile
                (if inMod > outMod
                 then
                     (do putStrLn $ "Compiling " ++ inFile ++ " to " ++ outFile
                         action )
                 else
                     return ExitSuccess))
        else
            (do putStrLn $ "Compiling " ++ inFile ++ " to " ++ outFile
                action ))

sysList cmds = do when debug (print cmd)
                  system cmd
    where cmd = unwords cmds

ghcLoadPath = "-ihs" </> "Base:hs" </> "Compiler:hs" </> "Model:hs" </> "Arrow:gen" </> "hs"
ghciLoadPath = ghcLoadPath ++ ":test" </> "Compiler"


ghcCall = "ghc --make " ++ ghcLoadPath ++ " -outputdir gen" </> "ghc"

ghc file = sysList [ghcCall, "-o", outFile, "-main-is", modName, inFile]
  where modName = takeBaseName file
        outFile = "gen" </> "bin" </> "" ++ map toLower modName
        inFile  = file ++ ".hs"

ghci = sysList ["rlwrap", "ghci", ghciLoadPath]

runCompiler comp inFile outFile =
    (whenNewer
     inFile
     outFile
     (sysList ["gen" </> "bin" </> comp, "<", inFile, "2>&1", ">", outFile] ))

hs2c   = runCompiler "hs2c"
cmp2hs = runCompiler "cmp2hs"

runBin = system . (("gen" </> "bin") </>)

testCompiler comp = do
  ghc    $ "test" </> "Compiler" </> "Test" ++ comp
  runBin $ "test" ++ map toLower comp

testCompilers = liftM maximum .  mapM testCompiler

test = testCompilers ["Haskell2Code", "Comp2Haskell"]

mkDirs = mapM $ createDirectoryIfMissing True

compLatex filename = (whenNewer 
                      (filename ++ ".tex")
                      (filename ++ ".pdf") 
                      (sysList ["comp-latex", filename]))

genDoc = do
  setCurrentDirectory "doc/images"
  system "find -name \"*.eps\" -exec epstopdf {} \\;"
  setCurrentDirectory ".."
  compLatex "diplomarbeit"
  compLatex "slides"
  setCurrentDirectory ".."
  return ExitSuccess   

clean = system "rm -rf gen"


build = do
  system "rm -rf gen/sep gen/hs"
  mkDirs ["gen" </> "bin", 
          "gen" </> "ghc", 
          "gen" </> "hs", 
          "gen" </> "sep" </> "Haskell"]
  ghc $ "hs" </> "Prog" </> "Format"
  ghc "hs/Prog/HS2C"
  -- hs2c "sep/Haskell/Comp2Haskell.sep" "gen/hs/Comp2Haskell.hs"
  ghc "hs/Prog/CMP2HS"
  cmp2hs "sep/Compiler/BaseCompiler.sep" "gen/sep/Haskell/BaseCompiler.sep"
  hs2c "gen/sep/Haskell/BaseCompiler.sep" "gen/hs/BaseCompiler.hs"


main = do args  <- getArgs
          case args of
            ["ghci"]                -> ghci
            ["test"]                -> test >> return ExitSuccess
--            "repl" : _              -> system "rlwrap gen/bin/repl"
            [comp, inFile, outFile] -> runCompiler comp inFile outFile
            ["clean"]               -> clean
            ["doc"]                 -> genDoc
            [] -> do
--              clean
              putStrLn "Building"
              buildRes <- build
              (if buildRes == ExitSuccess then
                   do putStrLn "Testing"
                      test
               else
                   return buildRes)
