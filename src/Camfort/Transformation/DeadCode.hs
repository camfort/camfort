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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Camfort.Transformation.DeadCode where

import Camfort.Analysis.Annotations
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran as F
import qualified Language.Fortran.Util.Position as FU
import qualified Language.Analysis as FA
import Camfort.Helpers

import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Generics.Uniplate.Operations
import Data.Maybe
import GHC.Generics

import Debug.Trace

-- Eliminate dead code from a program, based on the fortran-src
-- live-variable analysis

-- Currently only strips out dead code through simple variable assignments
-- but not through array-subscript assignmernts
deadCode :: Bool -> (Filename, F.ProgramFile A)
                 -> (Report, (Filename, F.ProgramFile A))
deadCode flag (fname, pf) = (fname, (report, pf'))
  where
    (report, pf'') = deadCode' lva pf'
    -- initialise analysis
    pf'   = FAB.analyseBBLocks . FAR.analyseRenames . FA.initAnalysis $ pf
    -- get map of program unit ==> basic block graph
    bbm   = FAB.genBBlockMap pf'
    -- build the supergraph of global dependency
    sgr   = FAB.genSuperBBGr bbm
    -- extract the supergraph itself
    gr    = FAB.superBBGrGraph sgr
    -- live variables
    lva   = FAD.liveVariableAnalysis gr

deadCode' :: Bool -> InOutMap (S.Set Name)
                  -> F.ProgramFile (FA.Analysis A)
                  -> (Report, (F.ProgramFile (FA.Analysis A)))
deadCode' flag lva pf =
    if null report
      then (report, pf')
      else (report, pf') >>= (deadCode' flag lva)
  where
    (report, pf') = mapM (transformBiM (perStmnt flag lva)) pf

-- Core of the transformation happens here on assignment statements
perStmt :: Bool -> InOutMap (S.Set Name) -> F.Statement A -> (Report, F.Statement A)
perStmt flag lva x@(F.StExpressionAssgign a sp@(FU.SrcSpan s1 s2) e1 e2)
     | pRefactored a == flag =
  fromMaybe ("", x) $
    do id <- FA.insLabel a
        (_, out) <- IM.lookup lva id
        assignedName <- varExprToVariableF e1
        if assignedName `S.elem` out
          then Just x
          else -- Dead assignment
            Just (report, F.StExpressionAssgign a' sp e1 e2)
              where report =  "o" ++ (show s1) ++ ": removed dead code\n"
                    -- Set annotation to mark statement for elimination in
                    -- the reprinter
                    a' = a { refactored = (Just s1) }) (dropLineF sp)
perStmt _ _ x = return x
