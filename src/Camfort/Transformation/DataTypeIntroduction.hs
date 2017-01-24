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

import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Map.Lazy as M

import qualified Data.Set as S

import Camfort.Helpers
import Camfort.Helpers.Syntax
import Camfort.Analysis.Annotations

import qualified Data.IntMap as IM

-- Array-subscript interference graphs, in a map from
-- the array variable to the interference graph
type IGraphs = M.Map F.Name (Gr F.Name Int)

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
analysePerPF (fname, pf) = (report, pf') = transformBiM (perStmt lva) pf
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

-- Core of the transformation happens here on assignment statements
perStmt :: -> FAD.InOutMap (S.Set F.Name)
           -> F.Statement (FA.Analysis A) -> State (IGraphs, F.Statement (FA.Analysis A))
perStmt lva x =
  case (FA.insLabel (FA.getAnnotation x)) of
    Just label -> case (IM.lookup label lva) of
      Just (lva_in, _) -> transforBiM (perStmt lva_in)

--perExpr :: FAD.InOutMap (S.Set F.Name)
--        -> F.Expression (FA.Analysis A) -> State (G F.Name F.Name) (F.Expression (FA.Analysis A))
perExpr lva_in x@(F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable arrVar)) subs) = do
  let subscript_vars = [v | (F.ValVariable v) <- subs]
  let intefering = [(v, w) | v <- subscript_vars,
                             w <- subscript_vars, v `S.member` lva_in && w `S.member` lva_in]
  igraphs <- get
  case (M.lookup igraphs arrVar) of
     Just igraph -> return x
          -- TODO: update grapg here
     Nothing -> do
        let g0 = G.mkGraph [(0, u),(1, v)] [(0, 1, ())]
        let m = M.fromList [(arrVar, g0)]
        put (m `M.union` igraphs)
        return x
perExpr _ _ x = return x
