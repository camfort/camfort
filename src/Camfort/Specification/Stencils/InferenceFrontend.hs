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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Camfort.Specification.Stencils.InferenceFrontend
  (
    -- * Datatypes and Aliases
    InferMode(..)
    -- * Functions
  , stencilInference
  ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict hiding (Product)

import Camfort.Analysis.CommentAnnotator
import Camfort.Specification.Stencils.CheckBackend (synToAst)
import Camfort.Specification.Stencils.CheckFrontend
  (CheckResult, existingStencils, stencilChecking)
import Camfort.Specification.Stencils.Generate
import Camfort.Specification.Stencils.InferenceBackend
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.Syntax
import qualified Camfort.Specification.Stencils.Grammar as Gram
import qualified Camfort.Specification.Stencils.Synthesis as Synth
import Camfort.Analysis.Annotations
import Camfort.Helpers (collect, descendReverseM, descendBiReverseM)
import qualified Camfort.Helpers.Vec as V
import Camfort.Input

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import Data.Data
import Data.Foldable
import Data.Generics.Uniplate.Operations
import Data.Graph.Inductive.Graph hiding (isEmpty)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Monoid ((<>))

-- Define modes of interaction with the inference
data InferMode =
  AssignMode | EvalMode | Synth
  deriving (Eq, Show, Data, Read)

instance Default InferMode where
    defaultValue = AssignMode

data InferState = IS {
     ivMap        :: FAD.InductionVarMapByASTBlock
   , visitedNodes :: [Int]}

data InferEnv = IE
  {
    -- | Known (existing) specifications.
    ieExistingSpecs :: [(Specification, FU.SrcSpan, Variable)]
  , ieFlowsGraph    :: FAD.FlowsGraph A
  }


-- The inferer returns information as a LogLine
type LogLine = (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))
-- The core of the inferer works within this monad
type Inferer = WriterT [LogLine]
                 (ReaderT InferEnv
                    (State InferState))

runInferer :: CheckResult
           -> FAD.InductionVarMapByASTBlock
           -> FAD.FlowsGraph A
           -> Inferer a
           -> (a, [LogLine])
runInferer cr ivmap flTo =
    flip evalState (IS ivmap [])
  . flip runReaderT (IE (existingStencils cr) flTo)
  . runWriterT

-- | Attempt to convert a 'Grammar.Specification' into a 'Specification'.
--
-- Only performs conversions for spatial specifications.
specToSynSpec :: Gram.SpecInner -> Maybe Specification
specToSynSpec spec = let ?renv = [] in
                       case synToAst spec of
                         Left err -> Nothing
                         Right x  -> Just x

-- | Main stencil inference code
stencilInference :: InferMode
                 -> Char
                 -> F.ProgramFile (FA.Analysis A)
                 -> (F.ProgramFile (FA.Analysis A), [LogLine])
stencilInference mode marker pf =
    (F.ProgramFile mi pus', log1)
  where
    -- Parse specification annotations and include them into the syntax tree
    -- that way if generate specifications at the same place we can
    -- decide whether to synthesise or not

    -- TODO: might want to output log0 somehow (though it doesn't fit LogLine)
    (pf'@(F.ProgramFile mi pus), _log0) =
         if mode == Synth
          then runWriter (annotateComments Gram.specParser pf)
          else (pf, [])

    (pus', log1)    = runWriter (transformBiM perPU pus)

    -- Run inference per program unit, placing the flowsmap in scope
    perPU :: F.ProgramUnit (FA.Analysis A)
          -> Writer [LogLine] (F.ProgramUnit (FA.Analysis A))

    perPU pu | Just _ <- FA.bBlocks $ F.getAnnotation pu = do
        let pum = descendBiM (perBlockInfer mode marker mi) pu
            checkRes = stencilChecking pf
            (pu', log) = runInferer checkRes ivMap flTo pum
            -- induction variable map
            ivMap = FAD.genInductionVarMapByASTBlock beMap gr
        tell log
        return pu'

    perPU pu = return pu

    -- perform reaching definitions analysis
    rd    = FAD.reachingDefinitions dm gr
    -- create graph of definition "flows"
    flTo =  FAD.genFlowsToGraph bm dm gr rd

    -- identify every loop by its back-edge
    beMap = FAD.genBackEdgeMap (FAD.dominators gr) gr

    -- get map of AST-Block-ID ==> corresponding AST-Block
    bm    = FAD.genBlockMap pf'
    -- get map of program unit ==> basic block graph
    bbm   = FAB.genBBlockMap pf'
    -- build the supergraph of global dependency
    sgr   = FAB.genSuperBBGr bbm
    -- extract the supergraph itself
    gr    = FAB.superBBGrGraph sgr

    -- get map of variable name ==> { defining AST-Block-IDs }
    dm    = FAD.genDefMap bm

{- *** 1 . Core inference over blocks -}

genSpecsAndReport ::
     InferMode -> FU.SrcSpan -> [Neighbour]
  -> [F.Block (FA.Analysis A)]
  -> Inferer [([Variable], Specification)]
genSpecsAndReport mode span lhsIxs blocks = do
    (IS ivmap _) <- get
    flowsGraph     <- fmap ieFlowsGraph ask
    -- Generate specification for the
    let ((specs, visited), evalInfos) = runWriter $ genSpecifications flowsGraph ivmap lhsIxs blocks
    -- Remember which nodes were visited during this traversal
    modify (\state -> state { visitedNodes = visitedNodes state ++ visited })
    -- Report the specifications
    tell [ (span, Left specs) ]

    -- Evaluation mode information reporting:
    when (mode == EvalMode) $ do
         tell [ (span, Right ("EVALMODE: assign to relative array subscript\
                              \ (tag: tickAssign)","")) ]
         forM_ evalInfos $ \evalInfo ->
             tell [ (span, Right evalInfo) ]
         forM_ specs $ \spec ->
             when (show spec == "") $
               tell [ (span, Right ("EVALMODE: Cannot make spec\
                                    \ (tag: emptySpec)","")) ]

    return specs



-- Traverse Blocks in the AST and infer stencil specifications
perBlockInfer :: InferMode -> Char -> F.MetaInfo
              -> F.Block (FA.Analysis A)
              -> Inferer (F.Block (FA.Analysis A))
perBlockInfer = perBlockInfer' False
-- The primed version, perBlockInfer' has a flag indicating whether
-- the following code is inside a do-loop since we only target
-- array computations inside loops.

perBlockInfer' _ _ _ _ b@F.BlComment{} = pure b

perBlockInfer' inDo mode marker mi b@(F.BlStatement ann span@(FU.SrcSpan lp _) _ stmnt) = do
    (IS ivmap visitedStmts) <- get
    let label = fromMaybe (-1) (FA.insLabel ann)
    if label `elem` visitedStmts
    then -- This statement has been part of a visited dataflow path
      return b
    else do
      -- On all StExpressionAssigns that occur in stmt....
      userSpecs <- fmap ieExistingSpecs ask
      let lhses = [lhs | (F.StExpressionAssign _ _ lhs _)
                           <- universe stmnt :: [F.Statement (FA.Analysis A)]]
      specs <- forM lhses $ \lhs ->
         case lhs of
          -- Assignment to a variable
          (F.ExpValue _ _ (F.ValVariable _)) | inDo ->
              genSpecsAndReport mode span [] [b]

          -- Assignment to something else...
          _ -> case isArraySubscript lhs of
             Just subs ->
               -- Left-hand side is a subscript-by relative index or by a range
               case neighbourIndex ivmap subs of
                 Just lhs -> genSpecsAndReport mode span lhs [b]
                 Nothing  -> if mode == EvalMode
                             then do
                               tell [(span , Right ("EVALMODE: LHS is an array\
                                                   \ subscript we can't handle \
                                                   \(tag: LHSnotHandled)",""))]
                               return []
                             else return []
             -- Not an assign we are interested in
             _ -> return []
      if mode == Synth && not (null specs) && specs /= [[]]
      then
        let specComment = Synth.formatSpec mi tabs marker (span, Left specs')
            specs' = concatMap (mapMaybe noSpecAlready) specs

            noSpecAlready (vars, spec) =
               if null vars'
               then Nothing
               else Just (vars', spec)
               where vars' = filter (\v -> (spec, span, v) `notElem` userSpecs) vars

            -- Indentation for the specification to match the code
            tabs  = FU.posColumn lp - 1
            (FU.SrcSpan loc _) = span
            span' = FU.SrcSpan (lp {FU.posColumn = 1}) (lp {FU.posColumn = 1})
            ann'  = ann { FA.prevAnnotation = (FA.prevAnnotation ann) { refactored = Just loc } }
        in pure (F.BlComment ann' span' (F.Comment specComment))
      else return b

perBlockInfer' _ mode marker mi b@(F.BlDo ann span lab cname lab' mDoSpec body tlab) = do
    -- descend into the body of the do-statement (in reverse order)
    body' <- mapM (descendBiReverseM (perBlockInfer' True mode marker mi)) (reverse body)
    return $ F.BlDo ann span lab cname lab' mDoSpec (reverse body') tlab

perBlockInfer' inDo mode marker mi b =
    -- Go inside child blocks
    descendReverseM (descendBiReverseM (perBlockInfer' inDo mode marker mi)) b

--------------------------------------------------

-- Cute <3
-- Penelope's first code, 20/03/2016.
-- iii././//////////////////////. mvnmmmmmmmmmu

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
