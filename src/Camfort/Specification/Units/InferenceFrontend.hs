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
  Units of measure extension to Fortran: frontend
-}

module Camfort.Specification.Units.InferenceFrontend
  ( runInferVariables
  ) where

import Control.Monad.State (get)

import           Camfort.Specification.Units.Environment
import           Camfort.Specification.Units.InferenceBackend
import           Camfort.Specification.Units.Monad

-- | Return a list of variable names mapped to their corresponding
-- unit that was inferred.
runInferVariables :: UnitSolver [(VV, UnitInfo)]
runInferVariables = do
  cons <- usConstraints <$> get
  return $ inferVariables cons
