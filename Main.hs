module Main where

import qualified Language.Fortran.Parser as Fortran
import Language.Fortran.PreProcess
import Language.Fortran

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
import Extensions.Units

import Analysis.Types
import Analysis.Loops
import Analysis.LVA
import Analysis.Syntax

import Helpers
import Output
import Traverse

import Debug.Trace

import Data.List (nub, (\\), elemIndices, intersperse)
import Data.Text (pack, unpack, split) -- hiding (replicate, concat, length, head, concatMap, map, filter, take, last, intersperse, zip)

-- * The main entry point to CamFort
{-| The entry point to CamFort. Displays user information, and handlers which functionality is being requested -}
main = do putStrLn introMessage 
          args <- getArgs 
          if (length args >= 2) then
             let (func:(dir:_)) = args
                 excludes = if (length args == 2) then [] else args !! 2
                 outdir   = if (length args <= 3) then dir else args !! 3
                 excluded_files = map unpack (split (==',') (pack excludes))
             in case (lookup func (analyses ++ refactorings)) of 
                  Just (fun, _) -> fun dir excluded_files outdir
                  Nothing ->
                      case func of 
                         "ast"   -> ast dir (args!!2) -- args!!2 is output file here
                         _       -> putStrLn $ usage ++ menu
           else putStrLn $ usage ++ menu

{-| List of refactorings provided in CamFort -}
refactorings :: [(String, (Directory -> [Filename] -> Directory -> IO (), String))]
refactorings = 
    [("common", (common, "common block elimination")),
     ("commonArg", (commonToArgs, "common block elimination (to parameter passing)")),
     ("equivalence", (equivalences, "equivalence elimination")),
     ("dataType", (typeStructuring, "derived data type introduction")),
     ("dead", (dead, "dead-code elimination")),
     ("units", (units, "unit-of-measure inference")),
     ("removeUnits", (unitRemoval, "unit-of-measure removal"))]
     
{-| List of analses provided by CamFort -}
analyses :: [(String, (Directory -> [Filename] -> Directory -> IO (), String))]
analyses = 
    [("asts", (asts, "blank analysis, outputs analysis files with AST information")), 
     ("lva", (lvaA, "live-variable analysis")),
     ("loops", (loops, "loop information")), 
     ("count", (countVarDecls, "count variable declarations")),
     ("criticalUnits", (unitCriticals, "calculate the critical variables for units-of-measure inference"))]

-- * Usage and about information
version = 0.615
introMessage = "CamFort " ++ (show version) ++ " - Cambridge Fortran Infrastructure."
usage = "Usage: camfort <function> <input-dir> [\"excluded-files, ...\"] [output-dir]\n\n"
menu = "Refactor functions:\n"
        ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ (replicate (15 - length k) ' ') 
        ++ "\t [" ++ info ++ "] \n") refactorings
        ++ "\nAnalysis functions:\n" 
        ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ (replicate (15 - length k) ' ') 
        ++ "\t [" ++ info ++ "] \n") analyses

-- * Wrappers on all of the features 

typeStructuring inDir excludes outDir = 
     do putStrLn $ "Introducing derived data types for source in directory " ++ show inDir ++ "\n"
        doRefactor typeStruct inDir excludes outDir

ast d f = do (_, _, p) <- readParseSrcFile (d ++ "/" ++ f)
             putStrLn $ show p

asts inDir excludes _ = 
     do putStrLn $ "Do a basic analysis and output the HTML files with AST information for " ++ show inDir ++ "\n"
        doAnalysis ((map numberStmts) . map (fmap (const unitAnnotation))) inDir excludes 

countVarDecls inDir excludes _ =  
    do putStrLn $ "Counting variable declarations in directory " ++ show inDir ++ "\n"
       doAnalysisSummary countVariableDeclarations inDir excludes 

loops inDir excludes _ =  
           do putStrLn $ "Analysing loops for source in directory " ++ show inDir ++ "\n"
              doAnalysis loopAnalyse inDir excludes 

lvaA inDir excludes _ =  
          do putStrLn $ "Analysing loops for source in directory " ++ show inDir ++ "\n"
             doAnalysis lva inDir excludes 

dead inDir excludes outDir =
         do putStrLn $ "Eliminating dead code for source in directory " ++ show inDir ++ "\n"
            doRefactor ((mapM (deadCode False))) inDir excludes outDir

commonToArgs inDir excludes outDir = 
                 do putStrLn $ "Refactoring common blocks for source in directory " ++ show inDir ++ "\n"
                    doRefactor (commonElimToCalls inDir) inDir excludes outDir

common inDir excludes outDir =  
           do putStrLn $ "Refactoring common blocks for source in directory " ++ show inDir ++ "\n"
              doRefactor (commonElimToModules inDir) inDir excludes outDir

equivalences inDir excludes outDir =
           do putStrLn $ "Refactoring equivalences blocks for source in directory " ++ show inDir ++ "\n"
              doRefactor (mapM refactorEquivalences) inDir excludes outDir

units inDir excludes outDir = 
          do putStrLn $ "Inferring units for source in directory " ++ show inDir ++ "\n"
             doRefactor (mapM inferUnits) inDir excludes outDir

unitRemoval inDir excludes outDir = 
          do putStrLn $ "Removing units for source in directory " ++ show inDir ++ "\n"
             doRefactor (mapM removeUnits) inDir excludes outDir

unitCriticals inDir excludes outDir = 
          do putStrLn $ "Infering critical variables for units inference in directory " ++ show inDir ++ "\n"
             doAnalysisReport (mapM inferCriticalVariables) inDir excludes outDir


-- * Builders for analysers and refactorings

{-| Performs an analysis provided by its first parameter on the directory of its second, excluding files listed by
    its third -}
doAnalysis :: (Program A -> Program Annotation) -> Directory -> [Filename] -> IO ()
doAnalysis aFun d excludes = 
                    do if excludes /= []
                           then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ d ++ "/"
                           else return ()
                      
                       ps <- readParseSrcDir d excludes

                       let inFiles = map Fortran.fst3 ps
                       let outFiles = filter (\f -> not ((take (length $ d ++ "out") f) == (d ++ "out"))) inFiles
                       let asts' = map (\(f, _, ps) -> aFun ps) ps
                       -- (show (map (map (fmap (const ()))) (map (\(_, _, f) -f) pss))) `trace`
                       outputAnalysisFiles d asts' outFiles

{-| Performs an analysis provided by its first parameter which generates information 's', which is then combined
    together (via a monoid) -}
doAnalysisSummary :: (Monoid s, Show s) => (Program A -> s) -> Directory -> [Filename] -> IO ()
doAnalysisSummary aFun d excludes = 
                    do if excludes /= []
                           then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ d ++ "/"
                           else return ()
                      
                       ps <- readParseSrcDir d excludes

                       let inFiles = map Fortran.fst3 ps
                       putStrLn "Output of the analysis:" 
                       putStrLn $ show $ Prelude.foldl (\n (f, _, ps) -> n `mappend` (aFun ps)) mempty ps

{-| Performs an analysis which reports to the user, but does not output any files -}
doAnalysisReport :: ([(Filename, Program A)] -> (String, t1)) -> Directory -> [Filename] -> t -> IO ()
doAnalysisReport rFun inDir excludes outDir
                  = do if excludes /= []
                           then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inDir ++ "/"
                           else return ()
                       ps <- readParseSrcDir inDir excludes
                       putStr "\n"
                       let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
                       putStrLn report

{-| Performs a refactoring provided by its first parameter, on the directory of the second, excluding files listed by third, 
 output to the directory specified by the fourth parameter -}
doRefactor :: ([(Filename, Program A)] -> (String, [(Filename, Program Annotation)])) -> Directory -> [Filename] -> Directory -> IO ()
doRefactor rFun inDir excludes outDir
                  = do if excludes /= []
                           then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inDir ++ "/"
                           else return ()

                       ps <- readParseSrcDir inDir excludes
                       let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
                       --let outFiles = filter (\f -not ((take (length $ d ++ "out") f) == (d ++ "out"))) (map fst ps')
                       let outFiles = map fst ps'
                       putStrLn report
                       outputFiles inDir outDir (zip3 outFiles (map Fortran.snd3 ps ++ (repeat "")) (map snd ps'))


-- * Source directory and file handling

{-| Read files from a direcotry, excluding those listed by the second parameter -}
readParseSrcDir :: Directory -> [Filename] -> IO [(Filename, SourceText, Program A)]
readParseSrcDir d excludes = do dirF <- rGetDirectoryContents d
                                let files = dirF \\ excludes
                                let files' = map (\y -> d ++ "/" ++ y) files
                                mapM readParseSrcFile files'

rGetDirectoryContents :: Directory -> IO [String]
rGetDirectoryContents d = do ds <- getDirectoryContents d
                             ds' <- return $ ds \\ [".", ".."] -- remove '.' and '..' entries
                             rec ds'
                             where 
                               rec []     = return $ []
                               rec (x:xs) = do xs' <- rec xs
                                               g <- doesDirectoryExist (d ++ "/" ++ x)
                                               if g then 
                                                  do x' <- rGetDirectoryContents (d ++ "/" ++ x)
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


{-| Creates a directory (from a filename string) if it doesn't exist -}
checkDir f = case (elemIndices '/' f) of 
               [] -> return ()
               ix -> let d = take (last ix) f
                     in createDirectoryIfMissing True d

{-| Given a directory and list of triples of filenames, with their source text (if it exists) and
   their AST, write these to the director -}
outputFiles :: Directory -> Directory -> [(Filename, SourceText, Program Annotation)] -> IO ()
outputFiles inDir outDir pdata = 
           do createDirectoryIfMissing True outDir
              putStrLn $ "Writing refactored files to directory: " ++ outDir ++ "/"
              mapM_ (\(f, inp, ast') -> let f' = changeDir outDir inDir f
                                        in do checkDir f'
                                              putStrLn $ "Writing " ++ f'
                                              writeFile f' (reprint inp f' ast')) pdata


{-| changeDir is used to change the directory of a filename string.
    If the filename string has no directory then this is an identity  -}
changeDir newDir oldDir oldFilename = newDir ++ (listDiffL oldDir oldFilename)
                                      where listDiffL []     ys = ys
                                            listDiffL xs     [] = []
                                            listDiffL (x:xs) (y:ys) | x==y      = listDiffL xs ys
                                                                    | otherwise = ys

{-| output pre-analysis ASTs into the directory with the given file names (the list of ASTs should match the
    list of filenames) -}
outputAnalysisFiles :: Directory -> [Program Annotation] -> [Filename] -> IO ()
outputAnalysisFiles dir asts files =
           do putStrLn $ "Writing analysis files to directory: " ++ dir ++ "/"
              mapM (\(ast', f) -> writeFile (f ++ ".html") ((concatMap outputHTML) ast')) (zip asts files)
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