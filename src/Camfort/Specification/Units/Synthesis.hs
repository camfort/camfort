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

{-# LANGUAGE PatternGuards, DoAndIfThenElse, ConstraintKinds, ScopedTypeVariables #-}

module Camfort.Specification.Units.Synthesis
  (runSynthesis)
where

import           Camfort.Analysis.Annotations
import           Camfort.Specification.Units.Analysis (puName, puSrcName)
import           Camfort.Specification.Units.Annotation (UA)
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.Environment
import           Camfort.Specification.Units.Monad
import           Control.Monad (foldM, forM, (<=<))
import           Control.Monad.State.Strict hiding (gets)
import           Data.Generics.Uniplate.Operations
import           Data.Maybe
import qualified Data.Set as S
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Util.Position as FU

-- | Insert unit declarations into the ProgramFile as comments.
runSynthesis :: Char -> [(VV, UnitInfo)] -> UnitSolver [(VV, UnitInfo)]
runSynthesis marker vars = do
  -- descendBiM finds the head of lists
  modifyProgramFileM $ descendBiM (synthProgramUnits marker vars) <=< descendBiM (synthBlocks marker vars)
  return vars

-- Should be invoked on the beginning of a list of blocks
synthBlocks :: Char -> [(VV, UnitInfo)] -> [F.Block UA] -> UnitSolver [F.Block UA]
synthBlocks marker vars = fmap reverse . foldM (synthBlock marker vars) []

-- Process an individual block while building up a list of blocks (in
-- reverse order) to ultimately replace the original list of
-- blocks. We're looking for blocks containing declarations, in
-- particular, in order to possibly insert a unit annotation before
-- them.
synthBlock :: Char -> [(VV, UnitInfo)] -> [F.Block UA] -> F.Block UA -> UnitSolver [F.Block UA]
synthBlock marker vars bs b@(F.BlStatement a (FU.SrcSpan lp _) _ (F.StDeclaration _ _ _ _ decls)) = do
  pf    <- usProgramFile `fmap` get
  gvSet <- usGivenVarSet `fmap` get
  newBs <- fmap catMaybes . forM (universeBi decls) $ \ e -> case e of
    (F.ExpValue _ _ (F.ValVariable _))
      | vname `S.notMember` gvSet                     -- not a member of the already-given variables
      , Just u <- lookup (vname, sname) vars -> do    -- and a unit has been inferred
        -- Create new annotation which labels this as a refactored node.
        let newA  = a { FA.prevAnnotation    = (FA.prevAnnotation a) {
                           UA.prevAnnotation = (UA.prevAnnotation (FA.prevAnnotation a)) {
                               refactored = Just lp } } }
            -- Create a zero-length span for the new comment node.
            newSS = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
            -- Build the text of the comment with the unit annotation.
            txt   = marker:" " ++ showUnitDecl (FA.srcName e, u)
            space = FU.posColumn lp - 1
            (F.ProgramFile mi _) = pf
            newB  = F.BlComment newA newSS . F.Comment $ buildCommentText mi space txt
        return $ Just newB
      where
        vname = FA.varName e
        sname = FA.srcName e
    (_ :: F.Expression UA) -> return Nothing
  return (b:reverse newBs ++ bs)
synthBlock _ _ bs b = return (b:bs)

-- Should be invoked on the beginning of a list of program units
synthProgramUnits :: Char -> [(VV, UnitInfo)] -> [F.ProgramUnit UA] -> UnitSolver [F.ProgramUnit UA]
synthProgramUnits marker vars pus = do
  fmap reverse . foldM (synthProgramUnit marker vars) [] $ pus

-- Process an individual program unit while building up a list of
-- program units (in reverse order) to ultimately replace the original
-- list of program units. We're looking for functions, in particular,
-- in order to possibly insert a unit annotation before them.
synthProgramUnit :: Char -> [(VV, UnitInfo)] -> [F.ProgramUnit UA] -> F.ProgramUnit UA -> UnitSolver [F.ProgramUnit UA]
synthProgramUnit marker vars pus pu@(F.PUFunction a (FU.SrcSpan lp _) _ _ _ _ mret _ _) = do
  pf    <- usProgramFile `fmap` get
  gvSet <- usGivenVarSet `fmap` get
  let (vname, sname) = case mret of Just e  -> (FA.varName e, FA.srcName e)
                                    Nothing -> (puName pu, puSrcName pu)
  case lookup (vname, sname) vars of
    -- if return var has a unit & not a member of the already-given variables
    Just u | vname `S.notMember` gvSet -> do
      let newA  = a { FA.prevAnnotation = (FA.prevAnnotation a) {
                         UA.prevAnnotation = (UA.prevAnnotation (FA.prevAnnotation a)) {
                             refactored = Just lp } } }
      -- Create a zero-length span for the new comment node.
      let newSS = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
      -- Build the text of the comment with the unit annotation.
          txt   = marker:" " ++ showUnitDecl (sname, u)
          space = FU.posColumn lp - 1
          (F.ProgramFile mi _) = pf
          newPU = F.PUComment newA newSS . F.Comment $ buildCommentText mi space txt

      -- recursively descend to find program units inside of current one
      fmap (:newPU:pus) $ descendBiM (synthProgramUnits marker vars) pu

    -- otherwise, nevermind, but still recursively descend to find
    -- program units inside of current one
    _ -> fmap (:pus) $ descendBiM (synthProgramUnits marker vars) pu
synthProgramUnit marker vars pus pu = fmap (:pus) $ descendBiM (synthProgramUnits marker vars) pu

-- Pretty print a unit declaration.
showUnitDecl :: ([Char], UnitInfo) -> [Char]
showUnitDecl (sname, u) = "unit(" ++ show u ++ ") :: " ++ sname
