{-# LANGUAGE ImplicitParams, DoAndIfThenElse #-}

module Main where

import qualified Language.Fortran.Parser as Fortran
import Language.Fortran.PreProcess
import Language.Fortran

import qualified Forpar.Parser.Fortran77 as F77
import qualified Forpar.AST as A

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO

import Language.Haskell.ParseMonad

import Data.Monoid
import Data.Generics.Uniplate.Operations

import Analysis.Annotations

import Transformation.DeadCode
import Transformation.CommonBlockElim
import Transformation.CommonBlockElimToCalls
import Transformation.EquivalenceElim
import Transformation.DerivedTypeIntro
import Extensions.UnitsForpar
import Extensions.UnitsEnvironment
import Extensions.UnitsSolve

import Analysis.Types
import Analysis.Loops
import Analysis.LVA
import Analysis.Syntax
import qualified Analysis.Stencils as Stencils
import qualified Analysis.StencilsForpar as StencilsForpar

import Helpers
import Output
import Traverse

import Debug.Trace

import Data.List (foldl', nub, (\\), elemIndices, intersperse)
import Data.Text (pack, unpack, split)



-- * The main entry point to CamFort
{-| The entry point to CamFort. Displays user information, and handlers which functionality
     is being requested -}
main = do putStrLn introMessage
          args <- getArgs

          if (length args >= 2) then

              let (func : (inp : _)) = args
              in case lookup func functionality of
                   Just (fun, _) ->
                     do (numReqArgs, outp) <- if (func `elem` outputNotRequired)
                                               then if (length args >= 3 && (head (args !! 2) == '-'))
                                                    then return (2, "")
                                                    else -- case where an unnecessary output is specified
                                                      return (3, "")

                                               else if (length args >= 3)
                                                    then return (3, args !! 2)
                                                    else error $ usage ++ "This mode requires an output file/directory to be specified."
                        (opts, _) <- compilerOpts (drop numReqArgs args)
                        let excluded_files = map unpack (split (==',') (pack (getExcludes opts)))
                        fun inp excluded_files outp opts
                   Nothing -> putStrLn $ fullUsageInfo

          else if (length args == 1) then putStrLn $ usage ++ "Please specify an input file/directory"
                                     else putStrLn $ fullUsageInfo

-- * Options for CamFort  and information on the different modes

fullUsageInfo = (usageInfo (usage ++ menu ++ "\nOptions:") options)

type Options = [Flag]
data Flag = Version | Input String | Output String
         | Solver Solver | Excludes String
         | Literals AssumeLiterals | Debug deriving Show

solverType [] = Custom
solverType ((Solver s) : _) = s
solverType (x : xs) = solverType xs

literalsBehaviour [] = Poly
literalsBehaviour ((Literals l) : _) = l
literalsBehaviour (x : xs) = literalsBehaviour xs

getExcludes [] = ""
getExcludes ((Excludes s) : xs) = s
getExcludes (x : xs) = getExcludes xs

options :: [OptDescr Flag]
options =
     [ Option ['v','?'] ["version"] (NoArg Version)       "show version number"
     , Option ['e']     ["exclude"] (ReqArg Excludes "FILES") "files to exclude (comma separated list, no spaces)"
     , Option ['s']     ["units-solver"]  (ReqArg (Solver . read) "ID") "units-of-measure solver. ID = Custom or LAPACK"
     , Option ['l']     ["units-literals"] (ReqArg (Literals . read) "ID") "units-of-measure literals mode. ID = Unitless, Poly, or Mixed"
     ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = introMessage ++ usage ++ menu ++ "\nOptions:"

-- * Which modes do not require an output
outputNotRequired = ["criticalUnits", "count"]

functionality = analyses ++ refactorings

{-| List of refactorings provided in CamFort -}
refactorings :: [(String, (FileOrDir -> [Filename] -> FileOrDir -> Options -> IO (), String))]
refactorings =
    [("common", (common, "common block elimination")),
     ("commonArg", (commonToArgs, "common block elimination (to parameter passing)")),
     ("equivalence", (equivalences, "equivalence elimination")),
     ("dataType", (typeStructuring, "derived data type introduction")),
     ("dead", (dead, "dead-code elimination")),
     ("units", (units, "unit-of-measure inference")),
     ("removeUnits", (unitRemoval, "unit-of-measure removal"))]

{-| List of analses provided by CamFort -}
analyses :: [(String, (FileOrDir -> [Filename] -> FileOrDir -> Options -> IO (), String))]
analyses =
    [("asts", (asts, "blank analysis, outputs analysis files with AST information")),
     ("lva", (lvaA, "live-variable analysis")),
     ("loops", (loops, "loop information")),
     ("count", (countVarDecls, "count variable declarations")),
     ("criticalUnits", (unitCriticals, "calculate the critical variables for units-of-measure inference")),
     ("ast", (ast, "print the raw AST -- for development purposes")),
     ("stencils-infer", (stencilsInf, "stencil spec inference")),
     ("stencils-check", (stencilsCheck, "stencil spec checking"))]

-- * Usage and about information
version = 0.615
introMessage = "CamFort " ++ (show version) ++ " - Cambridge Fortran Infrastructure."
usage = "Usage: camfort <MODE> <INPUT> [OUTPUT] [OPTIONS...]\n"
menu = "Refactor functions:\n"
        ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ (replicate (15 - length k) ' ')
        ++ "\t [" ++ info ++ "] \n") refactorings
        ++ "\nAnalysis functions:\n"
        ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ (replicate (15 - length k) ' ')
        ++ "\t [" ++ info ++ "] \n") analyses

-- * Wrappers on all of the features
typeStructuring inSrc excludes outSrc _ =
     do putStrLn $ "Introducing derived data types in " ++ show inSrc ++ "\n"
        doRefactor typeStruct inSrc excludes outSrc

ast d _ f _ = do (_, _, p) <- readParseSrcFile (d ++ "/" ++ f)
                 putStrLn $ show p

asts inSrc excludes _ _ =
     do putStrLn $ "Do a basic analysis and output the HTML files with AST information for " ++ show inSrc ++ "\n"
        doAnalysis ((map numberStmts) . map (fmap (const unitAnnotation))) inSrc excludes

countVarDecls inSrc excludes _ _ =
    do putStrLn $ "Counting variable declarations in " ++ show inSrc ++ "\n"
       doAnalysisSummary countVariableDeclarations inSrc excludes

stencilsInfOld inSrc excludes _ _ =
          do putStrLn $ "Inferring stencil specs for " ++ show inSrc ++ "\n"
             doAnalysisSummary Stencils.infer inSrc excludes

stencilsInf inSrc excludes _ _ =
          do putStrLn $ "Inferring stencil specs for " ++ show inSrc ++ "\n"
             doAnalysisSummaryForpar StencilsForpar.infer inSrc excludes

stencilsCheck inSrc excludes _ _ =
          do putStrLn $ "Checking stencil specs for " ++ show inSrc ++ "\n"
             doAnalysis Stencils.check inSrc excludes   

loops inSrc excludes _ _ =  
           do putStrLn $ "Analysing loops for " ++ show inSrc ++ "\n"
              doAnalysis loopAnalyse inSrc excludes

lvaA inSrc excludes _ _ =
          do putStrLn $ "Analysing loops for " ++ show inSrc ++ "\n"
             doAnalysis lva inSrc excludes

dead inSrc excludes outSrc _ =
         do putStrLn $ "Eliminating dead code in " ++ show inSrc ++ "\n"
            doRefactor ((mapM (deadCode False))) inSrc excludes outSrc

commonToArgs inSrc excludes outSrc _ =
                 do putStrLn $ "Refactoring common blocks in " ++ show inSrc ++ "\n"
                    doRefactor (commonElimToCalls inSrc) inSrc excludes outSrc

common inSrc excludes outSrc _ =
           do putStrLn $ "Refactoring common blocks in " ++ show inSrc ++ "\n"
              doRefactor (commonElimToModules inSrc) inSrc excludes outSrc

equivalences inSrc excludes outSrc _ =
           do putStrLn $ "Refactoring equivalences blocks in " ++ show inSrc ++ "\n"
              doRefactor (mapM refactorEquivalences) inSrc excludes outSrc

{- Units feature -}
units inSrc excludes outSrc opt =
          do putStrLn $ "Inferring units for " ++ show inSrc ++ "\n"
             let s0 = UnitState { solver = solverType opt, assumeLiterals = literalsBehaviour opt }
             doRefactorForpar (mapM (runUnits s0 . inferUnits)) inSrc excludes outSrc

unitRemoval inSrc excludes outSrc opt =
          do putStrLn $ "Removing units in " ++ show inSrc ++ "\n"
             let ?solver = solverType opt
              in let ?assumeLiterals = literalsBehaviour opt
                 in doRefactorForpar (mapM removeUnits) inSrc excludes outSrc

unitCriticals inSrc excludes outSrc opt = do
  putStrLn $ "Infering critical variables for units inference in directory " ++ show inSrc ++ "\n"
  let s0 = UnitState { solver = solverType opt, assumeLiterals = literalsBehaviour opt }
  doRefactorForpar (mapM (runUnits s0 . inferCriticalVariables)) inSrc excludes outSrc

-- * Builders for analysers and refactorings

{-| Performs an analysis provided by its first parameter on the directory of its second, excluding files listed by
    its third -}
doAnalysis :: (Program A -> Program Annotation) -> FileOrDir -> [Filename] -> IO ()
doAnalysis aFun d excludes =
                    do if excludes /= [] && excludes /= [""]
                           then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ d ++ "/"
                           else return ()

                       ps <- readParseSrcDir d excludes

                       let inFiles = map Fortran.fst3 ps
                       let outFiles = filter (\f -> not ((take (length $ d ++ "out") f) == (d ++ "out"))) inFiles
                       let asts' = map (\(f, _, ps) -> aFun ps) ps
                       outputAnalysisFiles d asts' outFiles

{-| Performs an analysis provided by its first parameter which generates information 's', which is then combined
    together (via a monoid) -}
doAnalysisSummary :: (Monoid s, Show' s) => (Program A -> s) -> FileOrDir -> [Filename] -> IO ()
doAnalysisSummary aFun d excludes = 
                    do if excludes /= [] && excludes /= [""]
                           then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ d ++ "/"
                           else return ()

                       ps <- readParseSrcDir d excludes

                       let inFiles = map Fortran.fst3 ps
                       putStrLn "Output of the analysis:" 
                       putStrLn $ show' $ Prelude.foldl (\n (f, _, ps) -> n `mappend` (aFun ps)) mempty ps

doAnalysisSummaryForpar :: (Monoid s, Show' s) => (A.ProgramFile A -> s) -> FileOrDir -> [Filename] -> IO ()
doAnalysisSummaryForpar aFun inSrc excludes = do
  if excludes /= [] && excludes /= [""]
    then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inSrc ++ "/"
    else return ()
  ps <- readForparseSrcDir inSrc excludes
  let inFiles = map Fortran.fst3 ps
  putStrLn "Output of the analysis:"
  putStrLn . show' $ foldl' (\n (f, _, ps) -> n `mappend` (aFun ps)) mempty ps

class Show' s where
      show' :: s -> String
instance Show' String where
      show' = id
instance Show' Int where
      show' = show

{-| Performs an analysis which reports to the user, but does not output any files -}
doAnalysisReport :: ([(Filename, Program A)] -> (String, t1)) -> FileOrDir -> [Filename] -> t -> IO ()
doAnalysisReport rFun inSrc excludes outSrc
                  = do if excludes /= [] && excludes /= [""]
                           then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inSrc ++ "/"
                           else return ()
                       ps <- readParseSrcDir inSrc excludes
                       putStr "\n"
                       let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
                       putStrLn report

{-| Performs an analysis which reports to the user, but does not output any files -}
doAnalysisReportForpar :: ([(Filename, A.ProgramFile A)] -> (String, t1)) -> FileOrDir -> [Filename] -> t -> IO ()
doAnalysisReportForpar rFun inSrc excludes outSrc = do
  if excludes /= [] && excludes /= [""]
      then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inSrc ++ "/"
      else return ()
  ps <- readForparseSrcDir inSrc excludes

  putStr "\n"
  let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
  putStrLn report

{-| Performs a refactoring provided by its first parameter, on the directory of the second, excluding files listed by third,
 output to the directory specified by the fourth parameter -}
doRefactor :: ([(Filename, Program A)] -> (String, [(Filename, Program Annotation)])) -> FileOrDir -> [Filename] -> FileOrDir -> IO ()
doRefactor rFun inSrc excludes outSrc
                  = do if excludes /= [] && excludes /= [""]
                           then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inSrc ++ "/"
                           else return ()

                       ps <- readParseSrcDir inSrc excludes
                       let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
                       --let outFiles = filter (\f -not ((take (length $ d ++ "out") f) == (d ++ "out"))) (map fst ps')
                       let outFiles = map fst ps'
                       putStrLn report
                       outputFiles inSrc outSrc (zip3 outFiles (map Fortran.snd3 ps ++ (repeat "")) (map snd ps'))

doRefactorForpar :: ([(Filename, A.ProgramFile A)] -> (String, [(Filename, A.ProgramFile Annotation)])) -> FileOrDir -> [Filename] -> FileOrDir -> IO ()
doRefactorForpar rFun inSrc excludes outSrc =
  do if excludes /= [] && excludes /= [""]
         then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inSrc ++ "/"
         else return ()

     ps <- readForparseSrcDir inSrc excludes
     let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
     --let outFiles = filter (\f -not ((take (length $ d ++ "out") f) == (d ++ "out"))) (map fst ps')
     let outFiles = map fst ps'
     putStrLn report
     -- outputFiles inSrc outSrc (zip3 outFiles (map Fortran.snd3 ps ++ (repeat "")) (map snd ps'))

-- gets the directory part of a filename
getDir :: String -> String
getDir file = take (last $ elemIndices '/' file) file

-- * Source directory and file handling

{-| Read files from a direcotry, excluding those listed by the second parameter -}
readParseSrcDir :: FileOrDir -> [Filename] -> IO [(Filename, SourceText, Program A)]
readParseSrcDir inp excludes = do isdir <- isDirectory inp
                                  files <- if isdir then
                                               do files <- rGetDirContents inp
                                                  return $ (map (\y -> inp ++ "/" ++ y) files) \\ excludes
                                           else return [inp]
                                  mapM readParseSrcFile files

readForparseSrcDir :: FileOrDir -> [Filename] -> IO [(Filename, SourceText, A.ProgramFile A)]
readForparseSrcDir inp excludes = do isdir <- isDirectory inp
                                     files <- if isdir then
                                                  do files <- rGetDirContents inp
                                                     return $ (map (\y -> inp ++ "/" ++ y) files) \\ excludes
                                              else return [inp]
                                     mapM readForparseSrcFile files

rGetDirContents :: FileOrDir -> IO [String]
rGetDirContents d = do ds <- getDirectoryContents d
                       ds' <- return $ ds \\ [".", ".."] -- remove '.' and '..' entries
                       rec ds'
                             where
                               rec []     = return $ []
                               rec (x:xs) = do xs' <- rec xs
                                               g <- doesDirectoryExist (d ++ "/" ++ x)
                                               if g then
                                                  do x' <- rGetDirContents (d ++ "/" ++ x)
                                                     return $ (map (\y -> x ++ "/" ++ y) x') ++ xs'
                                                else if (isFortran x) then
                                                         return $ x : xs'
                                                     else return $ xs'

{-| predicate on which fileextensions are Fortran files -}
isFortran x = elem (fileExt x) [".f", ".f90", ".f77", ".cmn", ".inc"]

{-| Read a specific file, and parse it -}
readParseSrcFile :: Filename -> IO (Filename, SourceText, Program A)
readParseSrcFile f = do putStrLn f
                        inp <- readFile f
                        ast <- parse f
                        return $ (f, inp, map (fmap (const unitAnnotation)) ast)

{-| Read a specific file, and parse it -}
readForparseSrcFile :: Filename -> IO (Filename, SourceText, A.ProgramFile A)
readForparseSrcFile f = do putStrLn f
                           inp <- readFile f
                           let ast = forparse inp f
                           return $ (f, inp, fmap (const unitAnnotation) ast)

{-| Creates a directory (from a filename string) if it doesn't exist -}
checkDir f = case (elemIndices '/' f) of
               [] -> return ()
               ix -> let d = take (last ix) f
                     in createDirectoryIfMissing True d

isDirectory :: FileOrDir -> IO Bool
isDirectory s = doesDirectoryExist s

{-| Given a directory and list of triples of filenames, with their source text (if it exists) and
   their AST, write these to the director -}
outputFiles :: FileOrDir -> FileOrDir -> [(Filename, SourceText, Program Annotation)] -> IO ()
outputFiles inp outp pdata =
  do outIsDir <- isDirectory outp
     inIsDir  <- isDirectory inp
     if outIsDir then
       do createDirectoryIfMissing True outp
          putStrLn $ "Writing refactored files to directory: " ++ outp ++ "/"
          isdir <- isDirectory inp
          let inSrc = if isdir then inp else getDir inp
          mapM_ (\(f, input, ast') -> let f' = changeDir outp inSrc f
                                      in do checkDir f'
                                            putStrLn $ "Writing " ++ f'
                                            writeFile f' (reprint input f' ast')) pdata
     else
         if inIsDir || length pdata > 1 then
             error $ "Error: attempting to output multiple files, but the given output destination " ++
                       "is a single file. \n" ++ "Please specify an output directory"
         else let outSrc = getDir outp
              in do createDirectoryIfMissing True outSrc
                    putStrLn $ "Writing refactored file to: " ++ outp
                    let (f, input, ast') = head pdata
                    putStrLn $ "Writing " ++ outp
                    writeFile outp (reprint input outp ast')

{-| changeDir is used to change the directory of a filename string.
    If the filename string has no directory then this is an identity  -}
changeDir newDir oldDir oldFilename = newDir ++ (listDiffL oldDir oldFilename)
                                      where listDiffL []     ys = ys
                                            listDiffL xs     [] = []
                                            listDiffL (x:xs) (y:ys) | x==y      = listDiffL xs ys
                                                                    | otherwise = ys

{-| output pre-analysis ASTs into the directory with the given file names (the list of ASTs should match the
    list of filenames) -}
outputAnalysisFiles :: FileOrDir -> [Program Annotation] -> [Filename] -> IO ()
outputAnalysisFiles dir asts files =
           do putStrLn $ "Writing analysis files to directory: " ++ dir ++ "/"
              mapM (\(ast', f) -> writeFile (f ++ ".html") ((concatMap outputHTMLA) ast')) (zip asts files)
              return ()


{-| parse file into an un-annotated Fortran AST -}
parse  :: Filename -> IO (Program ())
parse f = let mode = ParseMode { parseFilename = f }
              selectedParser = case (fileExt f) of
                                  ".cmn" -> Fortran.include_parser
                                  ".inc" -> Fortran.include_parser
                                  _      -> Fortran.parser
          in do inp <- readFile f
                case (runParserWithMode mode selectedParser (pre_process inp)) of
                   (ParseOk p)       -> return $ p
                   (ParseFailed l e) -> error e

{-| parse file into an un-annotated Fortran AST -}
forparse :: SourceText -> Filename -> A.ProgramFile ()
forparse contents f = F77.fortran77Parser contents f


{-| extract a filename's extension -}
fileExt x = let ix = elemIndices '.' x
            in if (length ix == 0) then ""
               else Prelude.drop (Prelude.last ix) x


-- * Simple example

{-|  A simple, sample transformation using the 'transformBi' function -}
fooTrans p = transformBi f p
                where f :: Fortran A1 -> Fortran A1
                      f p@(Call x sp e as) = Label True sp "10" p
                      f p@(Assg x sp e1 e2) = Label True sp "5" p
                      f p = p

{-| Parse a file and apply the 'fooTrans' transformation, outputting to the filename + .out -}
doFooTrans f = do inp <- readFile f
                  p <- parse f
                  let p' = fooTrans $ (map (fmap (const unitAnnotation)) p)
                  let out = reprint inp f p'
                  writeFile (f ++ ".out") out
                  return $ (out, p')

test = stencilsInf "samples/stencils/one.f90" [] () ()
