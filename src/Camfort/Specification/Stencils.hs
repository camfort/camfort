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

import           Camfort.Analysis.Annotations
import           Camfort.Analysis.Fortran
  (analysisInput, analysisResult, branchAnalysis)
import           Camfort.Helpers
import           Camfort.Specification.Stencils.Analysis (StencilsAnalysis)
import qualified Camfort.Specification.Stencils.Annotation as SA
import           Camfort.Specification.Stencils.CheckFrontend hiding (LogLine)
import           Camfort.Specification.Stencils.InferenceFrontend
import           Camfort.Specification.Stencils.Synthesis

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB

import Data.List


-- | Helper for retrieving analysed blocks.
getBlocks = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis . fmap SA.mkStencilAnnotation

--------------------------------------------------
--         Stencil specification inference      --
--------------------------------------------------

-- Top-level of specification inference
infer :: Bool
      -> Char
      -> StencilsAnalysis (F.ProgramFile Annotation) Report
infer useEval marker = do
  pf <- analysisInput
  report <- analysisResult <$> branchAnalysis (stencilInference useEval marker) (getBlocks pf)
  let filename = F.pfGetFilename pf
      output = intercalate "\n"
             . filter (not . white)
             . map formatSpecNoComment $ report
      white  = all (\x -> (x == ' ') || (x == '\t'))
    -- Append filename to any outputs
  pure . mkReport $ if null output then "" else "\n" ++ filename ++ "\n" ++ output

--------------------------------------------------
--         Stencil specification synthesis      --
--------------------------------------------------

-- Top-level of specification synthesis
synth :: Char
      -> StencilsAnalysis [F.ProgramFile A] (Report, [F.ProgramFile Annotation])
synth marker = do
  pfs <- analysisInput
  syntheses <- unzip <$> mapM (fmap analysisResult . branchAnalysis buildOutput) pfs
  let report = mkReport . normaliseMsg . fst $ syntheses
  pure (report, catMaybes $ snd syntheses)
  where
    buildOutput :: StencilsAnalysis (F.ProgramFile A) ((String, String), Maybe (F.ProgramFile Annotation))
    buildOutput = do
      pf <- analysisInput
      let f = F.pfGetFilename pf
      result <- synthWithCheck
      pure $ case result of
               Left err         -> ((mkMsg f err, ""), Nothing)
               Right (warn,pf') -> (("", mkMsg f warn), Just pf')
    synthWithCheck :: StencilsAnalysis (F.ProgramFile A) (Either String (String, F.ProgramFile Annotation))
    synthWithCheck = do
      pf <- analysisInput
      let blocks = getBlocks pf
      checkRes <- analysisResult <$> branchAnalysis stencilChecking blocks
      case checkFailure checkRes of
        Nothing  -> do
          res <- (snd . analysisResult) <$> branchAnalysis (stencilSynthesis marker) blocks
          let inference = fmap SA.getBaseAnnotation res
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

check :: StencilsAnalysis (F.ProgramFile Annotation) Report
check = do
  pf <- analysisInput
  res <- branchAnalysis stencilChecking (getBlocks pf)
  -- Append filename to any outputs
  let output   = show (analysisResult res)
      filename = F.pfGetFilename pf
  pure . mkReport $ if null output then "" else "\n" ++ filename ++ "\n" ++ output

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
