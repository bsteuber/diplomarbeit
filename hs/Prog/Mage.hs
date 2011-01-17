import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import System
import System.Environment (getArgs)
import System.Directory
import System.FilePath (takeBaseName, (</>))

-- debug = False
debug = True

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

sysCall str = do
  res <- system str
  (case res of 
     ExitFailure _ -> fail "An Error occurred"
     ExitSuccess   -> return ExitSuccess) 

sysList cmds = do when debug (print cmd)
                  sysCall cmd
    where cmd = unwords cmds

joinPaths = concat . intersperse ":"

ghcLoadPath = "-i" ++ joinPaths [--"gen" </> "hs" </> "Compiler",
                                 "hs" </> "Base",
                                 "hs" </> "Compiler",
                                 "hs" </> "Model",
                                 "hs" </> "Arrow"
                                ,"gen" </> "hs" </> "Compiler"
                                ]
ghciLoadPath = joinPaths [ghcLoadPath, "test" </> "Compiler"]


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

prep   = runCompiler "prep"
hs2c   = runCompiler "hs2c"
cmp2hs = runCompiler "cmp2hs"

runBin = sysCall . (("gen" </> "bin") </>)

testCompiler comp = do
  ghc    $ "test" </> "Compiler" </> "Test" ++ comp
  runBin $ "test" ++ map toLower comp

testCompilers = liftM maximum .  mapM testCompiler

test = testCompilers ["Haskell2Code", "Comp2Haskell"]

mkDirs = mapM $ createDirectoryIfMissing True

compLatex filename = (whenNewer 
                      (filename ++ ".tex")
                      (filename ++ ".pdf") 
                      (sysList ["pdflatex", filename]))

genDoc = do
  setCurrentDirectory "doc/images"
  sysCall "find -name \"*.eps\" -exec epstopdf {} \\;"
  setCurrentDirectory ".."
  compLatex "diplomarbeit"
  compLatex "slides"
  setCurrentDirectory ".."
  return ExitSuccess   

clean = sysCall "rm -rf gen"

deploy = do 
  sysCall "cp hs/Compiler/Comp2Haskell.hs hs/Compiler/Comp2Haskell.hs.sav"
  sysCall "cp gen/hs/Compiler/Comp2Haskell.hs hs/Compiler/Comp2Haskell.hs"

reploy = do 
  sysCall "mv hs/Compiler/Comp2Haskell.hs.sav hs/Compiler/Comp2Haskell.hs"

build = do
--  sysCall "rm -rf gen/sep gen/hs"
--  clean
  mkDirs ["gen" </> "bin", 
          "gen" </> "ghc", 
          "gen" </> "hs" </> "Compiler", 
          "gen" </> "sep" </> "Hask",
          "gen" </> "sep" </> "Comp"
         ]
  ghc $  "hs" </> "Prog" </> "Format"
  ghc    "hs/Prog/HS2C"
  ghc    "hs/Prog/CMP2HS"
  cmp2hs "sep/Comp/Preprocessor.sep"            "gen/sep/Hask/Preprocessor.sep"
  hs2c   "gen/sep/Hask/Preprocessor.sep"        "gen/hs/Compiler/Preprocessor.hs"
  ghc    "hs/Prog/Prep"                          
  prep   "sep/Comp/Comp2Haskell.sep"            "gen/sep/Comp/Comp2Haskell.sep"
  cmp2hs "gen/sep/Comp/Comp2Haskell.sep"        "gen/sep/Hask/Comp2Haskell.sep"
  hs2c   "gen/sep/Hask/Comp2Haskell.sep"        "gen/hs/Compiler/Comp2Haskell.hs"

-------------
-- Example --
-------------
                                                 
example = do                                       
  cmp2hs "example/Slide2Latex.cmp.sep"        "example/Slide2Latex.hs.sep"
  hs2c   "example/Slide2Latex.hs.sep"         "example/Slide2Latex.hs"
  ghc    "example/Slide2Latex"

  cmp2hs "example/Latex2Code.cmp.sep"        "example/Latex2Code.hs.sep"
  hs2c   "example/Latex2Code.hs.sep"         "example/Latex2Code.hs"
  ghc    "example/Latex2Code"

  runCompiler "slide2latex" "example/pres.sli.sep" "example/pres.tex.sep"
  runCompiler "latex2code" "example/pres.tex.sep" "example/pres.tex"
  setCurrentDirectory "example"
  compLatex "pres"
  setCurrentDirectory ".."
  return ExitSuccess

-------------
---- End ----
-------------

main = do args  <- getArgs
          case args of
            ["ghci"]                -> ghci
            ["build"]               -> build
            ["test"]                -> test >> return ExitSuccess
--            "repl" : _              -> sysCall "rlwrap gen/bin/repl"
            [comp, inFile, outFile] -> runCompiler comp inFile outFile
            ["clean"]               -> clean
            ["doc"]                 -> genDoc
            ["deploy"]              -> deploy
            ["example"]             -> example
            ["reploy"]              -> reploy
            [] -> do
              clean
              putStrLn "Building"
              buildRes <- build
              (if buildRes == ExitSuccess then
                   do putStrLn "Testing"
                      testRes <- test
                      (if testRes == ExitSuccess then
                           deploy
                       else
                           return testRes)
               else
                   return buildRes)
