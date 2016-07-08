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

{- This module collects together stubs that connect analysis/transformations
   with the input -> output procedures -}

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Camfort.Functionality where

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO

import Data.Monoid
import Data.Generics.Uniplate.Operations

import Camfort.Analysis.Annotations
import Camfort.Analysis.Types
import Camfort.Analysis.Loops
import Camfort.Analysis.LVA
import Camfort.Analysis.Syntax

import Camfort.Transformation.DeadCode
import Camfort.Transformation.CommonBlockElim
import Camfort.Transformation.CommonBlockElimToCalls
import Camfort.Transformation.EquivalenceElim
import Camfort.Transformation.DerivedTypeIntro

import Camfort.Extensions.Units as LU
import Camfort.Extensions.UnitSyntaxConversion
import Camfort.Extensions.UnitsEnvironment
import Camfort.Extensions.UnitsSolve

import Camfort.Helpers
import Camfort.Output
import Camfort.Input

import Data.Data
import Data.List (foldl', nub, (\\), elemIndices, intersperse, intercalate)

import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (replace)

-- FORPAR related imports
import qualified Language.Fortran.Parser.Any as FP
import qualified Language.Fortran.AST as A
import Language.Fortran.Analysis.Renaming
  (renameAndStrip, analyseRenames, unrename, NameMap)
import Language.Fortran.Analysis(initAnalysis)
import Camfort.Extensions.UnitsForpar
import qualified Camfort.Analysis.StencilSpecification as Stencils

-- CamFort optional flags
data Flag = Version
         | Input String
         | Output String
         | Solver Solver
         | Excludes String
         | Literals AssumeLiterals
         | StencilInferMode Stencils.InferMode
         | Debug deriving (Data, Show)

type Options = [Flag]

-- Extract excluces information from options
instance Default String where
    defaultValue = ""
getExcludes :: Options -> String
getExcludes xs = getOption xs

-- * Wrappers on all of the features
typeStructuring inSrc excludes outSrc _ = do
    putStrLn $ "Introducing derived data types in " ++ show inSrc ++ "\n"
    doRefactor typeStruct inSrc excludes outSrc

ast d _ f _ = do
    (_, _, p) <- readParseSrcFile (d ++ "/" ++ f)
    putStrLn $ show p

asts inSrc excludes _ _ = do
    putStrLn $ "Do a basic analysis and output the HTML files "
            ++ "with AST information for " ++ show inSrc ++ "\n"
    let astAnalysis = (map numberStmts) . map (fmap (const unitAnnotation))
    doAnalysis astAnalysis inSrc excludes

countVarDecls inSrc excludes _ _ = do
    putStrLn $ "Counting variable declarations in " ++ show inSrc ++ "\n"
    doAnalysisSummary countVariableDeclarations inSrc excludes

loops inSrc excludes _ _ = do
    putStrLn $ "Analysing loops for " ++ show inSrc ++ "\n"
    doAnalysis loopAnalyse inSrc excludes

lvaA inSrc excludes _ _ = do
    putStrLn $ "Analysing loops for " ++ show inSrc ++ "\n"
    doAnalysis lva inSrc excludes

dead inSrc excludes outSrc _ = do
    putStrLn $ "Eliminating dead code in " ++ show inSrc ++ "\n"
    doRefactor ((mapM (deadCode False))) inSrc excludes outSrc

commonToArgs inSrc excludes outSrc _ = do
    putStrLn $ "Refactoring common blocks in " ++ show inSrc ++ "\n"
    doRefactor (commonElimToCalls inSrc) inSrc excludes outSrc

common inSrc excludes outSrc _ = do
    putStrLn $ "Refactoring common blocks in " ++ show inSrc ++ "\n"
    doRefactor (commonElimToModules inSrc) inSrc excludes outSrc

equivalences inSrc excludes outSrc _ = do
    putStrLn $ "Refactoring equivalences blocks in " ++ show inSrc ++ "\n"
    doRefactor (mapM refactorEquivalences) inSrc excludes outSrc

{- Units feature -}
units inSrc excludes outSrc opt = do
    putStrLn $ "Inferring units for " ++ show inSrc ++ "\n"
    let ?solver = getOption opt :: Solver
     in let ?assumeLiterals = getOption opt :: AssumeLiterals
        in doRefactor' (mapM LU.inferUnits) inSrc excludes outSrc

unitCriticals inSrc excludes outSrc opt = do
    putStrLn $ "Infering critical variables for units inference in directory "
             ++ show inSrc ++ "\n"
    let ?solver = getOption opt :: Solver
     in let ?assumeLiterals = getOption opt :: AssumeLiterals
        in doAnalysisReport' (mapM LU.inferCriticalVariables)
              inSrc excludes outSrc

stencilsInf inSrc excludes outSrc opt = do
  putStrLn $ "Inferring stencil specs for " ++ show inSrc ++ "\n"
  doAnalysisSummaryForpar (Stencils.infer (getOption opt)) inSrc excludes (Just outSrc)

stencilsCheck inSrc excludes _ _ = do
  putStrLn $ "Checking stencil specs for " ++ show inSrc ++ "\n"
  doAnalysisSummaryForpar (\f p -> (Stencils.check f p, p)) inSrc excludes Nothing

stencilsVarFlowCycles inSrc excludes _ _ = do
  putStrLn $ "Inferring var flow cycles for " ++ show inSrc ++ "\n"
  let flowAnalysis = intercalate ", " . map show . Stencils.findVarFlowCycles
  doAnalysisSummaryForpar (\_ p -> (flowAnalysis p , p)) inSrc excludes Nothing

--------------------------------------------------
-- Forpar wrappers

doRefactorForpar :: ([(Filename, A.ProgramFile A)]
                 -> (String, [(Filename, A.ProgramFile Annotation)]))
                 -> FileOrDir -> [Filename] -> FileOrDir -> IO ()
doRefactorForpar rFun inSrc excludes outSrc = do
    if excludes /= [] && excludes /= [""]
    then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes)
           ++ " from " ++ inSrc ++ "/"
    else return ()
    ps <- readForparseSrcDir inSrc excludes
    let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
    --let outFiles = filter (\f -> not ((take (length $ d ++ "out") f) == (d ++ "out"))) (map fst ps')
    let outFiles = map fst ps'
    putStrLn report
  where snd3 (a, b, c) = b


{-| Performs an analysis which reports to the user,
     but does not output any files -}
doAnalysisReportForpar :: ([(Filename, A.ProgramFile A)] -> (String, t1))
                       -> FileOrDir -> [Filename] -> t -> IO ()
doAnalysisReportForpar rFun inSrc excludes outSrc = do
  if excludes /= [] && excludes /= [""]
      then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes)
                    ++ " from " ++ inSrc ++ "/"
      else return ()
  ps <- readForparseSrcDir inSrc excludes
----
  putStr "\n"
  let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
  putStrLn report
----

-- * Source directory and file handling
readForparseSrcDir :: FileOrDir -> [Filename]
                   -> IO [(Filename, SourceText, A.ProgramFile A)]
readForparseSrcDir inp excludes = do
    isdir <- isDirectory inp
    files <- if isdir
             then do files <- rGetDirContents inp
                     return $ (map (\y -> inp ++ "/" ++ y) files) \\ excludes
             else return [inp]
    mapM readForparseSrcFile files
----

{-| Read a specific file, and parse it -}
readForparseSrcFile :: Filename -> IO (Filename, SourceText, A.ProgramFile A)
readForparseSrcFile f = do
    inp <- flexReadFile f
    let ast = FP.fortranParser inp f
    return $ (f, inp, fmap (const unitAnnotation) ast)
----

doAnalysisSummaryForpar :: (Monoid s, Show' s) => (Filename -> A.ProgramFile A -> (s, A.ProgramFile A))
                        -> FileOrDir -> [Filename] -> Maybe FileOrDir -> IO ()
doAnalysisSummaryForpar aFun inSrc excludes outSrc = do
  if excludes /= [] && excludes /= [""]
    then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes)
                                 ++ " from " ++ inSrc ++ "/"
    else return ()
  ps <- readForparseSrcDir inSrc excludes
  let (out, ps') = callAndSummarise aFun ps
  putStrLn "Output of the analysis:"
  putStrLn . show' $ out

callAndSummarise aFun ps = do
  foldl' (\(n, pss) (f, _, ps) -> let (n', ps') = aFun f ps
                                  in (n `mappend` n', ps' : pss)) (mempty, []) ps

----

-- | Read file using ByteString library and deal with any weird characters.
flexReadFile :: String -> IO B.ByteString
flexReadFile = fmap (encodeUtf8 . decodeUtf8With (replace ' ')) . B.readFile
