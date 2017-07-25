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

module Camfort.Specification.Stencils
 (infer, check, synth) where

import Control.Arrow ((***), first, second)
import Data.Maybe (catMaybes)

import Camfort.Specification.Stencils.CheckFrontend hiding (LogLine)
import Camfort.Specification.Stencils.InferenceFrontend
import Camfort.Specification.Stencils.Synthesis
import Camfort.Analysis.Annotations
import Camfort.Analysis.Fortran
  (Analysis, analysisInput, analysisResult, branchAnalysis)
-- These two are redefined here for ForPar ASTs
import Camfort.Helpers

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB

import Data.List


-- | Helper for retrieving analysed blocks.
getBlocks = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis

--------------------------------------------------
--         Stencil specification inference      --
--------------------------------------------------

-- Top-level of specification inference
infer :: Bool
      -> Char
      -> Analysis (F.ProgramFile Annotation) String
infer useEval marker = do
  pf <- analysisInput
  report <- analysisResult <$> branchAnalysis (stencilInference useEval marker) (getBlocks pf)
  let filename = F.pfGetFilename pf
      output = intercalate "\n"
             . filter (not . white)
             . map formatSpecNoComment $ report
      white  = all (\x -> (x == ' ') || (x == '\t'))
    -- Append filename to any outputs
  pure $ if null output then "" else "\n" ++ filename ++ "\n" ++ output

--------------------------------------------------
--         Stencil specification synthesis      --
--------------------------------------------------

-- Top-level of specification synthesis
synth :: Char
      -> Analysis [F.ProgramFile A] (String, [F.ProgramFile Annotation])
synth marker = do
  pfs <- analysisInput
  syntheses <- unzip <$> mapM (fmap analysisResult . branchAnalysis buildOutput) pfs
  let report = normaliseMsg (fst syntheses)
  pure (report, catMaybes $ snd syntheses)
  where
    buildOutput :: Analysis (F.ProgramFile A) ((String, String), Maybe (F.ProgramFile Annotation))
    buildOutput = do
      pf <- analysisInput
      let f = F.pfGetFilename pf
      result <- synthWithCheck
      pure $ case result of
               Left err         -> ((mkMsg f err, ""), Nothing)
               Right (warn,pf') -> (("", mkMsg f warn), Just pf')
    synthWithCheck :: Analysis (F.ProgramFile A) (Either String (String, F.ProgramFile Annotation))
    synthWithCheck = do
      pf <- analysisInput
      let blocks = getBlocks pf
          checkRes = stencilChecking blocks
      case checkFailure checkRes of
        Nothing  -> do
          res <- (snd . analysisResult) <$> branchAnalysis (stencilSynthesis marker) blocks
          let inference = fmap FA.prevAnnotation res
          pure $ Right (maybe "" show (checkWarnings checkRes), inference)
        Just err -> pure . Left $ show err

    mkMsg _ "" = ""
    mkMsg f e  = "\nEncountered the following errors when checking\
                 \ stencil specs for '" ++ f ++ "'\n\n" ++ e

    normaliseMsg outs =
      let errors = fmap fst outs
          fullMsg = concatMap (uncurry (++)) outs
      in if any (/="") errors then fullMsg ++ errorTrailer else fullMsg
      where errorTrailer = "\nPlease resolve these errors, and then\
                           \ run synthesis again."


--------------------------------------------------
--         Stencil specification checking       --
--------------------------------------------------

check :: Analysis (F.ProgramFile Annotation) String
check = do
  pf <- analysisInput
  -- Append filename to any outputs
  let output   = show . stencilChecking . getBlocks $ pf
      filename = F.pfGetFilename pf
  pure $ if null output then "" else "\n" ++ filename ++ "\n" ++ output

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
