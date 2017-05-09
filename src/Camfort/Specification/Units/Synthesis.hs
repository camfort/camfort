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

{-# LANGUAGE PatternGuards, ScopedTypeVariables, ImplicitParams, DoAndIfThenElse, ConstraintKinds #-}

module Camfort.Specification.Units.Synthesis
  (runSynthesis)
where

import Data.Function
import Data.List
import Data.Matrix
import Data.Maybe
import Data.Ratio (numerator, denominator)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Strict hiding (gets)
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Util.Position as FU
import Language.Fortran.ParserMonad (FortranVersion(Fortran90))

import qualified Camfort.Specification.Units.Parser as P
import Camfort.Analysis.CommentAnnotator
import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.Monad
import Camfort.Specification.Units.InferenceFrontend (puName, puSrcName)
import qualified Debug.Trace as D

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
synthBlock marker vars bs b@(F.BlStatement a ss@(FU.SrcSpan lp up) _ (F.StDeclaration _ _ _ _ decls)) = do
  pf    <- usProgramFile `fmap` get
  gvSet <- usGivenVarSet `fmap` get
  newBs <- fmap catMaybes . forM (universeBi decls) $ \ e -> case e of
    e@(F.ExpValue _ _ (F.ValVariable _))
      | vname `S.notMember` gvSet                     -- not a member of the already-given variables
      , Just u <- lookup (vname, sname) vars -> do    -- and a unit has been inferred
        -- Create new annotation which labels this as a refactored node.
        let newA  = a { FA.prevAnnotation = (FA.prevAnnotation a) {
                           prevAnnotation = (prevAnnotation (FA.prevAnnotation a)) {
                               refactored = Just lp } } }
        -- Create a zero-length span for the new comment node.
        let newSS = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
        -- Build the text of the comment with the unit annotation.
        let txt   = marker:" " ++ showUnitDecl (FA.srcName e, u)
        let space = FU.posColumn lp - 1
        let newB  = F.BlComment newA newSS . F.Comment . insertSpacing pf space $ commentText pf txt
        return $ Just newB
      where
        vname = FA.varName e
        sname = FA.srcName e
    (e :: F.Expression UA) -> return Nothing
  return (b:reverse newBs ++ bs)
synthBlock _ _ bs b = return (b:bs)

-- Should be invoked on the beginning of a list of program units
synthProgramUnits :: Char -> [(VV, UnitInfo)] -> [F.ProgramUnit UA] -> UnitSolver [F.ProgramUnit UA]
synthProgramUnits marker vars = fmap reverse . foldM (synthProgramUnit marker vars) []

-- Process an individual program unit while building up a list of
-- program units (in reverse order) to ultimately replace the original
-- list of program units. We're looking for functions, in particular,
-- in order to possibly insert a unit annotation before them.
synthProgramUnit :: Char -> [(VV, UnitInfo)] -> [F.ProgramUnit UA] -> F.ProgramUnit UA -> UnitSolver [F.ProgramUnit UA]
synthProgramUnit marker vars pus pu@(F.PUFunction a ss@(FU.SrcSpan lp up) _ _ _ _ mret _ _) = do
  D.traceM $ show vars
  pf    <- usProgramFile `fmap` get
  gvSet <- usGivenVarSet `fmap` get
  let (vname, sname) = case mret of Just e  -> (FA.varName e, FA.srcName e)
                                    Nothing -> (puName pu, puSrcName pu)
  D.traceM $ show (vname, sname)
  case lookup (vname, sname) vars of
    -- if return var has a unit & not a member of the already-given variables
    Just u | vname `S.notMember` gvSet -> do
      let newA  = a { FA.prevAnnotation = (FA.prevAnnotation a) {
                         prevAnnotation = (prevAnnotation (FA.prevAnnotation a)) {
                             refactored = Just lp } } }
      -- Create a zero-length span for the new comment node.
      let newSS = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
      -- Build the text of the comment with the unit annotation.
      let txt   = marker:" " ++ showUnitDecl (sname, u)
      let space = FU.posColumn lp - 1
      let newPU = F.PUComment newA newSS . F.Comment . insertSpacing pf space $ commentText pf txt
      return (pu:newPU:pus)

    -- otherwise, nevermind
    _ -> return (pu:pus)
synthProgramUnit _ _ pus pu = return (pu:pus)

-- Insert the correct comment markers around the given text string, depending on Fortran version.
commentText :: F.ProgramFile UA -> String -> String
commentText pf text | isModernFortran pf = "!" ++ text
                    | otherwise          = "c" ++ text

-- Insert a given amount of spacing before the string.
insertSpacing :: F.ProgramFile UA -> Int -> String -> String
insertSpacing pf n | isModernFortran pf = (replicate n ' ' ++)
                   | otherwise          = id

-- Pretty print a unit declaration.
showUnitDecl (sname, u) = "unit(" ++ show u ++ ") :: " ++ sname

isModernFortran (F.ProgramFile (F.MetaInfo { F.miVersion = v }) _ _) = v >= Fortran90
