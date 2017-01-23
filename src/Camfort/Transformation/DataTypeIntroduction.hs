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

module Camfort.Transformation.DataTypeIntroduction where

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Util.Position as FU
import qualified Language.Fortran.ParserMonad as PM
import qualified Language.Fortran.PrettyPrint as PP

import qualified Data.Set as S

import Camfort.Helpers
import Camfort.Helpers.Syntax
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
analysePerPF (fname, pf) = lva
  where
    -- initialise analysis
    pf'   = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis $ pf
    -- get map of program unit ==> basic block graph
    bbm   = FAB.genBBlockMap pf'
    -- build the supergraph of global dependency
    sgr   = FAB.genSuperBBGr bbm
    -- extract the supergraph itself
    gr    = FAB.superBBGrGraph sgr
    -- live variables
    lva   = FAD.liveVariableAnalysis gr