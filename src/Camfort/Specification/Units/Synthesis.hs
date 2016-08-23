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

import qualified Camfort.Specification.Units.Parser as P
import Camfort.Analysis.CommentAnnotator
import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.Monad
import qualified Debug.Trace as D

-- | Insert unit declarations into the ProgramFile as comments.
runSynthesis :: Bool -> [(String, UnitInfo)] -> UnitSolver [(String, UnitInfo)]
runSynthesis doxygenEnabled vars = do
  modifyProgramFileM $ descendBiM (synthBlocks doxygenEnabled vars)   -- descendBiM finds the head of lists
  return vars

-- Should be invoked on the beginning of a list of blocks
synthBlocks :: Bool -> [(String, UnitInfo)] -> [F.Block UA] -> UnitSolver [F.Block UA]
synthBlocks doxygenEnabled vars =
    fmap reverse . foldM (synthBlock doxygenEnabled vars) []

-- Process an individual block while building up a list of blocks (in
-- reverse order) to ultimately replace the original list of
-- blocks. We're looking for blocks containing declarations, in
-- particular, in order to possibly insert a unit annotation before
-- them.
synthBlock :: Bool -> [(String, UnitInfo)] -> [F.Block UA] -> F.Block UA -> UnitSolver [F.Block UA]
synthBlock doxygenEnabled vars bs b@(F.BlStatement a ss@(FU.SrcSpan lp up) _ (F.StDeclaration _ _ _ _ decls)) = do
  pf    <- usProgramFile `fmap` get
  nMap  <- uoNameMap `fmap` ask
  gvSet <- usGivenVarSet `fmap` get
  newBs <- fmap catMaybes . forM (universeBi decls) $ \ e -> case e of
    e@(F.ExpValue _ _ (F.ValVariable _))
      | name `S.notMember` gvSet            -- not a member of the already-given variables
      , Just u <- lookup name vars -> do    -- and a unit has been inferred
        -- Create new annotation which labels this as a refactored node.
        let newA  = a { FA.prevAnnotation = (FA.prevAnnotation a) {
                           prevAnnotation = (prevAnnotation (FA.prevAnnotation a)) {
                               refactored = Just lp } } }
        -- Create a zero-length span for the new comment node.
        let newSS = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
        -- Build the text of the comment with the unit annotation.
        let marker = if doxygenEnabled then '!' else '='
        let txt   = marker:" " ++ showUnitDecl nMap (e, u)
        let space = FU.posColumn lp - 1
        let newB  = F.BlComment newA newSS . insertSpacing space $ commentText pf txt
        return $ Just newB
      where
        name = FA.varName e
    (e :: F.Expression UA) -> return Nothing
  return (b:reverse newBs ++ bs)
synthBlock _ _ bs b = return (b:bs)

-- Insert the correct comment markers around the given text string, depending on Fortran version.
-- FIXME: use Fortran meta information when I have finished adding it to ProgramFile.
commentText :: F.ProgramFile UA -> String -> String
commentText _ text = "!" ++ text

-- Insert a given amount of spacing before the string.
insertSpacing :: Int -> String -> String
insertSpacing n = (replicate n ' ' ++)

-- Pretty print a unit declaration.
showUnitDecl nameMap (e, u) = "unit(" ++ show u ++ ") :: " ++ (v `fromMaybe` M.lookup v nameMap)
  where v = FA.varName e
