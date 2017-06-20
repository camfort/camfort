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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Camfort.Transformation.DataTypeIntroduction
  ( dataTypeIntro
  ) where

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis.DataFlow as FAD


import qualified Data.Set as S

import Camfort.Helpers
import Camfort.Analysis.Annotations

import qualified Data.IntMap as IM

-- Top-level
dataTypeIntro ::
  [(Filename, F.ProgramFile A)] -> (Report, [(Filename, F.ProgramFile A)])
dataTypeIntro pfs = (r, [])
  where
    r = buildInterferenceGraph pfs

-- Stub, coalesce LVA information
-- TODO, build interference graph
buildInterferenceGraph :: [(Filename, F.ProgramFile A)] -> String
buildInterferenceGraph = show . (foldr IM.union IM.empty) . map analysePerPF

-- Stub, generate LVA information
analysePerPF ::
   (Filename, F.ProgramFile A) -> FAD.InOutMap (S.Set F.Name)
analysePerPF (_, _) = undefined
