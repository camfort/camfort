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

module Camfort.Specification.Stencils where
 -- (infer, check, synth) where

import Control.Arrow ((***), first, second)
import Data.Maybe (catMaybes)

import           Camfort.Analysis
import           Camfort.Analysis.Annotations
import           Camfort.Helpers
import           Camfort.Specification.Stencils.Analysis (StencilsAnalysis)
import qualified Camfort.Specification.Stencils.Annotation as SA
import           Camfort.Specification.Stencils.CheckFrontend hiding (LogLine)
import           Camfort.Specification.Stencils.InferenceFrontend
import           Camfort.Specification.Stencils.Synthesis

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Util.ModFile as MF
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
      -> F.ProgramFile Annotation
      -> MF.ModFiles
      -> StencilsAnalysis ()
infer useEval marker pf mfs = do
  -- report <- analysisResult <$> branchAnalysis (stencilInference useEval marker) (getBlocks pf)
  logs <- stencilInference useEval marker (getBlocks pf) mfs
  let filename = F.pfGetFilename pf
      output = intercalate "\n"
             . filter (not . white)
             . map formatSpecNoComment $ logs
      white  = all (\x -> (x == ' ') || (x == '\t'))

  logInfo' pf $ describe output

--------------------------------------------------
--         Stencil specification synthesis      --
--------------------------------------------------

-- Top-level of specification synthesis
synth :: Char
      -> [F.ProgramFile A]
      -> MF.ModFiles
      -> StencilsAnalysis ([F.ProgramFile Annotation])
synth marker pfs mfs = do
  syntheses <- unzip <$> traverse buildOutput pfs
  logInfo' pfs $ describe . normaliseMsg . fst $ syntheses
  pure (catMaybes $ snd syntheses)
  where
    buildOutput :: F.ProgramFile A -> StencilsAnalysis ((String, String), Maybe (F.ProgramFile Annotation))
    buildOutput pf = do
      let f = F.pfGetFilename pf
      result <- synthWithCheck pf
      pure $ case result of
               Left err         -> ((mkMsg f err, ""), Nothing)
               Right (warn,pf') -> (("", mkMsg f warn), Just pf')
    synthWithCheck :: F.ProgramFile A -> StencilsAnalysis (Either String (String, F.ProgramFile Annotation))
    synthWithCheck pf = do
      let blocks = getBlocks pf
      checkRes <- stencilChecking blocks mfs
      case checkFailure checkRes of
        Nothing  -> do
          res <- fst <$> stencilSynthesis marker blocks mfs
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

check :: F.ProgramFile Annotation -> MF.ModFiles -> StencilsAnalysis CheckResult
check pf mfs = stencilChecking (getBlocks pf) mfs

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
