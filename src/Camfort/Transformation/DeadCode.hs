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
{-# LANGUAGE OverloadedStrings #-}

module Camfort.Transformation.DeadCode
  ( deadCode
  ) where

import Camfort.Analysis
import Camfort.Analysis.Annotations
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU
import qualified Language.Fortran.Analysis as FA
import Camfort.Helpers.Syntax

import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Generics.Uniplate.Operations
import Data.Maybe
import Control.Monad (guard)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Monoid (Any(..), (<>))
import Data.Void (Void)

type DeadCodeAnalysis = PureAnalysis Void Void


-- Eliminate dead code from a program, based on the fortran-src
-- live-variable analysis

-- Currently only strips out dead code through simple variable assignments
-- but not through array-subscript assignmernts
deadCode :: Bool -> F.ProgramFile A -> DeadCodeAnalysis (F.ProgramFile A)
deadCode flag pf = do
  let
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

  deadCode' flag lva pf'
  pure $ fmap FA.prevAnnotation pf'

deadCode' :: Bool -> FAD.InOutMap (S.Set F.Name)
                  -> F.ProgramFile (FA.Analysis A)
                  -> DeadCodeAnalysis (F.ProgramFile (FA.Analysis A))
deadCode' flag lva pf = do
  (pf', Any eliminated) <- runWriterT $ transformBiM (perStmt flag lva) pf
  if eliminated
    then deadCode' flag lva pf'
    else return pf'

-- Core of the transformation happens here on assignment statements
perStmt :: Bool
        -> FAD.InOutMap (S.Set F.Name)
        -> F.Statement (FA.Analysis A) -> WriterT Any DeadCodeAnalysis (F.Statement (FA.Analysis A))
perStmt flag lva x@(F.StExpressionAssign a sp@(FU.SrcSpan s1 _) e1 e2)
     | pRefactored (FA.prevAnnotation a) == flag =
       let output = do
             logInfo' x $ "o" <> describe (show s1) <> ": removed dead code"
             tell (Any True)
             return $ F.StExpressionAssign a' (dropLine sp) e1 e2
               where a' = onPrev (\ap -> ap {refactored = Just s1}) a
                    -- Set annotation to mark statement for elimination in
                    -- the reprinter
       in maybe (return x) (const output) $ do
         label <- FA.insLabel a
         (_, out) <- IM.lookup label lva
         assignedName <- extractVariable e1
         guard (not (assignedName `S.member` out))
perStmt _ _ x = return x
