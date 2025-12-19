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
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Camfort.Specification.Stencils.InferenceFrontend
  (
    -- * Functions
    stencilInference
  , stencilSynthesis
    -- * report
  , StencilsReport(..)
  ) where

import           Camfort.Analysis
import           Camfort.Analysis.Annotations
import           Camfort.Analysis.CommentAnnotator
import           Camfort.Helpers (descendReverseM, descendBiReverseM)
import           Camfort.Specification.Stencils.Analysis (StencilsAnalysis)
import           Camfort.Specification.Stencils.Annotation (SA)
import qualified Camfort.Specification.Stencils.Annotation as SA
import           Camfort.Specification.Stencils.CheckFrontend
  (CheckResult, existingStencils, stencilChecking)
import           Camfort.Specification.Stencils.Generate
import qualified Camfort.Specification.Stencils.Parser as Parser
import           Camfort.Specification.Stencils.Syntax
import qualified Camfort.Specification.Stencils.Synthesis as Synth
import           Control.DeepSeq
import           Control.Monad (when)
import           Control.Monad.RWS.Lazy hiding (state)
import           Control.Monad.Writer.Lazy
import           Data.Foldable
import           Data.Generics.Uniplate.Operations
import qualified Data.Map as M
import           Data.Maybe
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU
import           Prelude hiding (span, log)

data InferState = IS {
     _ivMap       :: FAD.InductionVarMapByASTBlock
   , visitedNodes :: [Int]}

data InferEnv = IE
  {
    -- | Known (existing) specifications.
    ieExistingSpecs :: [(Specification, FU.SrcSpan, Variable)]
  , ieFlowsGraph    :: FAD.FlowsGraph (SA.StencilAnnotation A)
  -- | Provide additional evaluation information when active.
  , ieUseEval       :: Bool
  -- | Instruct the inferer to perform synthesis.
  , ieDoSynth       :: Bool
  , ieMarker        :: Char
  , ieMetaInfo      :: F.MetaInfo
  }


-- The inferer returns information as a LogLine
type LogLine = (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))

data StencilsReport = StencilsReport [(String, LogLine)] -- ^ (filename, logged stencil)

instance NFData StencilsReport where
  rnf _ = ()

instance ExitCodeOfReport StencilsReport where
  exitCodeOf _ = 0

instance Show StencilsReport where
  show (StencilsReport flogs) = unlines . filter (not . white) $ output
    where
      output = [ Synth.formatSpecNoComment ll | (_, ll) <- flogs ]
      white  = all (\x -> (x == ' ') || (x == '\t'))

instance Describe StencilsReport

-- The core of the inferer works within this monad

type Inferer = RWST InferEnv [LogLine] InferState StencilsAnalysis

getExistingSpecs :: Inferer [(Specification, FU.SrcSpan, Variable)]
getExistingSpecs = asks ieExistingSpecs

getFlowsGraph :: Inferer (FAD.FlowsGraph (SA.StencilAnnotation A))
getFlowsGraph = asks ieFlowsGraph

getMetaInfo :: Inferer F.MetaInfo
getMetaInfo = asks ieMetaInfo

getMarker :: Inferer Char
getMarker = asks ieMarker

getUseEval :: Inferer Bool
getUseEval = asks ieUseEval

getDoSynth :: Inferer Bool
getDoSynth = asks ieDoSynth

runInferer :: CheckResult
           -> Bool
           -> Bool
           -> Char
           -> F.MetaInfo
           -> FAD.InductionVarMapByASTBlock
           -> FAD.FlowsGraph (SA.StencilAnnotation A)
           -> Inferer a
           -> StencilsAnalysis (a, [LogLine])
runInferer cr useEval doSynth marker mi ivmap flTo inferer = do
  evalRWST inferer env (IS ivmap [])
  where env = IE
          { ieExistingSpecs = existingStencils cr
          , ieFlowsGraph    = flTo
          , ieUseEval       = useEval
          , ieDoSynth       = doSynth
          , ieMarker        = marker
          , ieMetaInfo      = mi
          }

-- | Run something only when eval mode is active.
whenEval :: Inferer () -> Inferer ()
whenEval i = getUseEval >>= (`when` i)

-- | Run something only when we should perform synthesis.
ifSynth :: Inferer a -> Inferer a -> Inferer a
ifSynth t e = getDoSynth >>= (\doSynth -> if doSynth then t else e)

-- | Attempt to convert a 'Parser.Specification' into a 'Specification'.
--
-- Only performs conversions for spatial specifications.
-- specToSynSpec :: SpecInner -> Maybe Specification
-- specToSynSpec spec = let ?renv = [] in
--                        case synToAst spec of
--                          Left _   -> Nothing
--                          Right x  -> Just x

-- | Main stencil inference code
stencilInference :: Bool
                 -> Char
                 -> F.ProgramFile SA
                 -> StencilsAnalysis [LogLine]
stencilInference useEval marker pf = execWriterT $ stencilSynthesis' useEval False marker pf

stencilSynthesis :: Char
                 -> F.ProgramFile SA
                 -> StencilsAnalysis (F.ProgramFile SA, [LogLine])
stencilSynthesis marker pf = do
  let (pf', _log0 :: String) = runWriter (annotateComments Parser.specParser (const . const . pure $ ()) pf)
  logDebug' pf $ describe _log0
  runWriterT $ stencilSynthesis' False True marker pf'

-- | Main stencil synthesis code
stencilSynthesis' :: Bool
                  -> Bool
                  -> Char
                  -> F.ProgramFile SA
                  -> WriterT [LogLine] StencilsAnalysis
                     (F.ProgramFile SA)
stencilSynthesis' useEval doSynth marker pf@(F.ProgramFile mi pus) = do
  checkRes <- lift $ stencilChecking pf

  let
    -- get map of AST-Block-ID ==> corresponding AST-Block
    bm    = FAD.genBlockMap pf
    -- get map of program unit ==> basic block graph
    bbm   = FAB.genBBlockMap pf
    -- get map of variable name ==> { defining AST-Block-IDs }
    dm    = FAD.genDefMap bm

    -- -- Run inference per program unit
    perPU :: F.ProgramUnit SA
          -> WriterT [LogLine] StencilsAnalysis (F.ProgramUnit SA)
    perPU pu | Just _ <- FA.bBlocks $ F.getAnnotation pu = do
        let -- Analysis/infer on blocks of just this program unit
            blocksM = mapM perBlockInfer (F.programUnitBody pu)
            -- Update the program unit body with these blocks
            pum = F.updateProgramUnitBody pu <$> blocksM

            -- perform reaching definitions analysis
            rd = FAD.reachingDefinitions dm gr

            Just gr = M.lookup (FA.puName pu) bbm
            -- create graph of definition "flows"
            flTo = FAD.genFlowsToGraph bm dm gr rd

            -- induction variable map
            beMap = FAD.genBackEdgeMap (FAD.dominators gr) $ FA.bbgrGr gr

            -- identify every loop by its back-edge
            ivmap = FAD.genInductionVarMapByASTBlock beMap gr

        (pu', log) <- lift $ runInferer checkRes useEval doSynth marker mi ivmap flTo pum
        tell log
        pure pu'
    perPU pu = pure pu

  pus' <- transformBiM perPU pus

  pure (F.ProgramFile mi pus')

{- *** 1 . Core inference over blocks -}

genSpecsAndReport ::
     FU.SrcSpan -> [Neighbour]
  -> F.Block SA
  -> Inferer [([Variable], Specification)]

genSpecsAndReport span lhsIxs block = do
  -- Get the induction variables relative to the current block
  (IS ivmap _) <- get
  let ivs = extractRelevantIVS ivmap block
  flowsGraph   <- getFlowsGraph
  -- Generate specification for the
  ((specs, visited), evalInfos) <- lift $ runStencilInferer (genSpecifications lhsIxs block) ivs flowsGraph
  -- Remember which nodes were visited during this traversal
  modify (\state -> state { visitedNodes = visitedNodes state ++ visited })
  -- Report the specifications
  tell [ (span, Left specs) ]

  -- Evaluation mode information reporting:
  whenEval $ do
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
perBlockInfer :: F.Block SA
              -> Inferer (F.Block SA)
perBlockInfer = perBlockInfer' False
-- The primed version, perBlockInfer' has a flag indicating whether
-- the following code is inside a do-loop since we only target
-- array computations inside loops.

perBlockInfer' :: Bool -> F.Block SA -> Inferer (F.Block SA)
perBlockInfer' _ b@F.BlComment{} = pure b

perBlockInfer' inDo b@(F.BlStatement ann span@(FU.SrcSpan lp _) _ stmnt) = do
  (IS ivmap visitedStmts) <- get
  let label = fromMaybe (-1) (FA.insLabel ann)
  if label `elem` visitedStmts
  then -- This statement has been part of a visited dataflow path
    return b
  else do
    -- On all StExpressionAssigns that occur in stmt....
    userSpecs <- getExistingSpecs
    let lhses = [lhs | (F.StExpressionAssign _ _ lhs _)
                         <- universe stmnt :: [F.Statement SA]]
    specs <- mapM (genSpecsFor ivmap) lhses
    marker <- getMarker
    mi     <- getMetaInfo
    ifSynth
      (if not (null specs) && specs /= [[]]
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
             ann'  = SA.modifyBaseAnnotation (\ba -> ba { refactored = Just loc }) ann
         in pure (F.BlComment ann' span' (F.Comment specComment))
       else pure b)
       (pure b)
  where
    -- Assignment to a variable
    genSpecsFor :: FAD.InductionVarMapByASTBlock -> F.Expression SA -> Inferer [([Variable], Specification)]
    genSpecsFor _ (F.ExpValue _ _ (F.ValVariable _)) | inDo = genSpecsAndReport span [] b
    -- Assignment to something else...
    genSpecsFor ivmap lhs =
      case isArraySubscript lhs of
        Just subs ->
          -- Left-hand side is a subscript-by relative index or by a range
          case neighbourIndex ivmap subs of
            Just lhs' -> genSpecsAndReport span lhs' b
            Nothing   -> do
              whenEval $
                tell [(span , Right ("EVALMODE: LHS is an array\
                                     \ subscript we can't handle \
                                     \(tag: LHSnotHandled)",""))]
              pure []
        -- Not an assign we are interested in
        _ -> pure []

perBlockInfer' _ (F.BlDo ann span lab cname lab' mDoSpec body tlab) = do
  -- descend into the body of the do-statement (in reverse order)
  body' <- mapM (descendBiReverseM (perBlockInfer' True)) (reverse body)
  return $ F.BlDo ann span lab cname lab' mDoSpec (reverse body') tlab

perBlockInfer' inDo b =
  -- Go inside child blocks
  descendReverseM (descendBiReverseM (perBlockInfer' inDo)) b

--------------------------------------------------

-- Cute <3
-- Penelope's first code, 20/03/2016.
-- iii././//////////////////////. mvnmmmmmmmmmu

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
