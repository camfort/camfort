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

{-# LANGUAGE ImplicitParams, DoAndIfThenElse #-}

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
import qualified Camfort.Analysis.StencilSpecification.LangFort as Stencils

import Camfort.Transformation.DeadCode
import Camfort.Transformation.CommonBlockElim
import Camfort.Transformation.CommonBlockElimToCalls
import Camfort.Transformation.EquivalenceElim
import Camfort.Transformation.DerivedTypeIntro

import Camfort.Extensions.Units
import Camfort.Extensions.UnitSyntaxConversion
import Camfort.Extensions.UnitsEnvironment
import Camfort.Extensions.UnitsSolve



import Camfort.Output
import Camfort.Input



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
             let ?solver = solverType opt 
              in let ?assumeLiterals = literalsBehaviour opt
                 in doRefactor' (mapM inferUnits) inSrc excludes outSrc

unitCriticals inSrc excludes outSrc opt = 
          do putStrLn $ "Infering critical variables for units inference in directory " ++ show inSrc ++ "\n"
             let ?solver = solverType opt 
              in let ?assumeLiterals = literalsBehaviour opt
                 in doAnalysisReport' (mapM inferCriticalVariables) inSrc excludes outSrc


-- * Wrappers on all of the features
stencilsInf inSrc excludes _ _ =
          do putStrLn $ "Inferring stencil specs for " ++ show inSrc ++ "\n"
             doAnalysisSummary Stencils.infer inSrc excludes


stencilsCheck inSrc excludes _ _ =
          do putStrLn $ "Checking stencil specs for " ++ show inSrc ++ "\n"
             doAnalysis Stencils.check inSrc excludes
