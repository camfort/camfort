{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, ImplicitParams, DoAndIfThenElse, PatternGuards #-}

module Main where

import qualified Language.Fortran.Parser as Fortran
import Language.Fortran.PreProcess
import Language.Fortran

-- FORPAR
-- import qualified Forpar.Parser.Fortran77 as F77
-- import qualified Forpar.AST as A
-- import Forpar.Analysis.Renaming(renameAndStrip, analyseRenames, unrename, NameMap)
-- import Forpar.Analysis(initAnalysis)
-- import Extensions.UnitsForpar
-- import qualified Analysis.StencilsForpar as StencilsForpar

import Data.Generics.Uniplate.Operations
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import Transformation.DeadCode
import Transformation.CommonBlockElim
import Transformation.CommonBlockElimToCalls
import Transformation.EquivalenceElim
import Transformation.DerivedTypeIntro

import Extensions.UnitsEnvironment
import Extensions.UnitsSolve

import Analysis.Annotations hiding (Unitless)
import Analysis.Types
import Analysis.Loops
import Analysis.LVA
import Analysis.Syntax
import qualified Analysis.Stencils as Stencils

import Helpers
import Output
import Input
import Functionality

import System.Console.GetOpt
import System.Environment
import System.IO

import Debug.Trace

import Data.List (foldl', nub, (\\), elemIndices, intersperse)
import Data.Text (pack, unpack, split)
import qualified Data.Map as M
import Data.Maybe

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
     ("units", (units, "unit-of-measure inference")) ]

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
version = 0.750
introMessage = "CamFort " ++ (show version) ++ " - Cambridge Fortran Infrastructure."
usage = "Usage: camfort <MODE> <INPUT> [OUTPUT] [OPTIONS...]\n"
menu = "Refactor functions:\n"
        ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ (replicate (15 - length k) ' ')
        ++ "\t [" ++ info ++ "] \n") refactorings
        ++ "\nAnalysis functions:\n"
        ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ (replicate (15 - length k) ' ')
        ++ "\t [" ++ info ++ "] \n") analyses

-- * Wrappers on all of the features
stencilsInf inSrc excludes _ _ =
          do putStrLn $ "Inferring stencil specs for " ++ show inSrc ++ "\n"
             doAnalysisSummary Stencils.infer inSrc excludes


stencilsCheck inSrc excludes _ _ =
          do putStrLn $ "Checking stencil specs for " ++ show inSrc ++ "\n"
             doAnalysis Stencils.check inSrc excludes


-- * Builders for analysers and refactorings


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

{-| Creates a directory (from a filename string) if it doesn't exist -}
checkDir f = case (elemIndices '/' f) of
               [] -> return ()
               ix -> let d = take (last ix) f
                     in createDirectoryIfMissing True d

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




-- * Simple example

{-| Parse a file and apply the 'fooTrans' transformation, outputting to the filename + .out -}
doFooTrans f = do inp <- readFile f
                  p <- parse f
                  let p' = fooTrans $ (map (fmap (const unitAnnotation)) p)
                  let out = reprint inp f p'
                  writeFile (f ++ ".out") out
                  return $ (out, p')

test = stencilsInf "samples/stencils/one.f90" [] () ()

--------------------------------------------------
-- Forpar stuff

---- stencilsInf inSrc excludes _ _ =
----           do putStrLn $ "Inferring stencil specs for " ++ show inSrc ++ "\n"
----              doAnalysisSummaryForpar StencilsForpar.infer inSrc excludes
----
---- stencilsFlow inSrc excludes _ _ = do
----   putStrLn $ "Flows for " ++ show inSrc ++ "\n"
----   doAnalysisSummaryForpar showFlow inSrc excludes
----   where
----     showFlow pf = unlines . flip map puFlows $ \ (pu, flmap) ->
----                     let flows = M.toList (descendBi fixName flmap) in
----                       show (descendBi fixName (A.getName pu)) ++ "\n" ++
----                       unlines (map (("\t"++) . show) flows)
----       where
----         (pf', nm) = renameAndStrip . analyseRenames . initAnalysis $ pf
----         puFlows   = StencilsForpar.flowAnalysisArrays pf'
----         fixName   :: String -> String
----         fixName n = n `fromMaybe` M.lookup n nm
----
---- doAnalysisSummaryForpar :: (Monoid s, Show' s) => (A.ProgramFile A -> s) -> FileOrDir -> [Filename] -> IO ()
---- doAnalysisSummaryForpar aFun inSrc excludes = do
----   if excludes /= [] && excludes /= [""]
----     then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inSrc ++ "/"
----     else return ()
----   ps <- readForparseSrcDir inSrc excludes
----   let inFiles = map Fortran.fst3 ps
----   putStrLn "Output of the analysis:"
----   putStrLn . show' $ foldl' (\n (f, _, ps) -> n `mappend` (aFun ps)) mempty ps
----
---- doRefactorForpar :: ([(Filename, A.ProgramFile A)] -> (String, [(Filename, A.ProgramFile Annotation)])) -> FileOrDir -> [Filename] -> FileOrDir -> IO ()
---- doRefactorForpar rFun inSrc excludes outSrc =
----   do if excludes /= [] && excludes /= [""]
----          then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inSrc ++ "/"
----          else return ()
----
----      ps <- readForparseSrcDir inSrc excludes
----      let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
----      --let outFiles = filter (\f -not ((take (length $ d ++ "out") f) == (d ++ "out"))) (map fst ps')
----      let outFiles = map fst ps'
----      putStrLn report
----      -- outputFiles inSrc outSrc (zip3 outFiles (map Fortran.snd3 ps ++ (repeat "")) (map snd ps'))
----
---- {-| Performs an analysis which reports to the user, but does not output any files -}
---- doAnalysisReportForpar :: ([(Filename, A.ProgramFile A)] -> (String, t1)) -> FileOrDir -> [Filename] -> t -> IO ()
---- doAnalysisReportForpar rFun inSrc excludes outSrc = do
----   if excludes /= [] && excludes /= [""]
----       then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes) ++ " from " ++ inSrc ++ "/"
----       else return ()
----   ps <- readForparseSrcDir inSrc excludes
----
----   putStr "\n"
----   let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
----   putStrLn report
----
---- -- * Source directory and file handling
---- readForparseSrcDir :: FileOrDir -> [Filename] -> IO [(Filename, SourceText, A.ProgramFile A)]
---- readForparseSrcDir inp excludes = do isdir <- isDirectory inp
----                                      files <- if isdir then
----                                                   do files <- rGetDirContents inp
----                                                      return $ (map (\y -> inp ++ "/" ++ y) files) \\ excludes
----                                               else return [inp]
----                                      mapM readForparseSrcFile files
----
---- {-| Read a specific file, and parse it -}
---- readParseSrcFile :: Filename -> IO (Filename, SourceText, Program A)
---- readParseSrcFile f = do putStrLn f
----                         inp <- readFile f
----                         ast <- parse f
----                         return $ (f, inp, map (fmap (const unitAnnotation)) ast)
----
---- {-| Read a specific file, and parse it -}
---- readForparseSrcFile :: Filename -> IO (Filename, SourceText, A.ProgramFile A)
---- readForparseSrcFile f = do putStrLn f
----                            inp <- readFile f
----                            let ast = forparse inp f
----                            return $ (f, inp, fmap (const unitAnnotation) ast)
----
---- {-| parse file into an un-annotated Fortran AST -}
---- forparse :: SourceText -> Filename -> A.ProgramFile ()
---- forparse contents f = F77.fortran77Parser contents f
