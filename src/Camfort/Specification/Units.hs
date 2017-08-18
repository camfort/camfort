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

{-

  Units of measure extension to Fortran

-}

module Camfort.Specification.Units (synthesiseUnits) where

import Control.Monad.Reader (asks)

import qualified Language.Fortran.AST      as F
import qualified Language.Fortran.Analysis as FA

import           Camfort.Analysis.Annotations
import           Camfort.Specification.Units.Analysis
  (UnitAnalysis, runInference)
import           Camfort.Specification.Units.Analysis.Consistent
  (ConsistencyError)
import           Camfort.Specification.Units.Analysis.Infer
  (InferenceReport, InferenceResult(..), getInferred, inferUnits)
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.InferenceBackend (chooseImplicitNames)
import           Camfort.Specification.Units.Monad
import           Camfort.Specification.Units.MonadTypes (UnitEnv(..))
import           Camfort.Specification.Units.Synthesis (runSynthesis)

{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits
  :: Char -> UnitAnalysis (Either ConsistencyError (InferenceReport, F.ProgramFile Annotation))
synthesiseUnits marker = do
  infRes <- inferUnits
  pfOriginal <- asks unitProgramFile
  case infRes of
    InfInconsistent err       -> pure $ Left err
    Inferred report -> do
      (_, state) <- runInference
        (runSynthesis marker . chooseImplicitNames . getInferred $ report)
      let pfUA    = usProgramFile state -- the program file after units analysis is done
          pfFinal = fmap (UA.prevAnnotation . FA.prevAnnotation) pfUA -- strip annotations
      pure . Right $ (report, pfFinal)
