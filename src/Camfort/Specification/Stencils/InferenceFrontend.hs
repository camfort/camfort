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
  DoMode | AssignMode | CombinedMode | EvalMode | Synth
  deriving (Eq, Show, Data, Read)

instance Default InferMode where
    defaultValue = AssignMode

data InferState = IS {
     ivMap        :: FAD.InductionVarMapByASTBlock
     -- ^ Known (existing) specifications.
   , hasSpec      :: [(Specification, FU.SrcSpan, Variable)]
   , visitedNodes :: [Int]}


-- The inferer returns information as a LogLine
type LogLine = (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))
-- The core of the inferer works within this monad
type Inferer = WriterT [LogLine]
                 (ReaderT (FAD.FlowsGraph A)
                    (State InferState))

runInferer :: FAD.InductionVarMapByASTBlock
           -> FAD.FlowsGraph A
           -> Inferer a
           -> (a, [LogLine])
runInferer ivmap flTo =
    flip evalState (IS ivmap [] [])
  . flip runReaderT flTo
  . runWriterT

-- | Add a new 'Specification' to the tracked specifications.
addSpec :: Specification -> FU.SrcSpan -> Variable -> Inferer ()
addSpec spec src var =
  modify (\s -> s { hasSpec = hasSpec s ++ [(spec, src, var)] })

-- | Add a new 'Grammar.Specification' to the tracked specifications.
addGSpec :: Gram.SpecInner -> FU.SrcSpan -> Variable -> Inferer ()
addGSpec spec src var = case specToSynSpec spec of
                          Nothing -> pure ()
                          Just spec' -> addSpec spec' src var

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
        let (pu', log) = runInferer ivMap flTo pum
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
    (IS ivmap _ _) <- get
    flowsGraph     <- ask
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

perBlockInfer _ _ _ b@F.BlComment{} = pure b

perBlockInfer mode marker mi b@(F.BlStatement ann span@(FU.SrcSpan lp _) _ stmnt)
  | mode == AssignMode || mode == CombinedMode || mode == EvalMode || mode == Synth = do

    addUserSpecsToTracked b
    (IS ivmap hasSpec visitedStmts) <- get
    let label = fromMaybe (-1) (FA.insLabel ann)
    if label `elem` visitedStmts
    then -- This statement has been part of a visited dataflow path
      return b
    else do
      -- On all StExpressionAssigns that occur in stmt....
      let lhses = [lhs | (F.StExpressionAssign _ _ lhs _)
                           <- universe stmnt :: [F.Statement (FA.Analysis A)]]
      specs <- forM lhses $ \lhs ->
         case lhs of
          -- Assignment to a variable
          (F.ExpValue _ _ (F.ValVariable _)) ->
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
               where vars' = filter (\v -> (spec, span, v) `notElem` hasSpec) vars

            -- Indentation for the specification to match the code
            tabs  = FU.posColumn lp - 1
            (FU.SrcSpan loc _) = span
            span' = FU.SrcSpan (lp {FU.posColumn = 1}) (lp {FU.posColumn = 1})
            ann'  = ann { FA.prevAnnotation = (FA.prevAnnotation ann) { refactored = Just loc } }
        in pure (F.BlComment ann' span' (F.Comment specComment))
      else return b

perBlockInfer mode marker mi b@(F.BlDo ann span lab cname lab' mDoSpec body tlab) = do
    addUserSpecsToTracked b
    if (mode == DoMode || mode == CombinedMode) && isStencilDo b
      then genSpecsAndReport mode span [] body
      else return []

    -- descend into the body of the do-statement (in reverse order)
    body' <- mapM (descendBiReverseM (perBlockInfer mode marker mi)) (reverse body)
    return $ F.BlDo ann span lab cname lab' mDoSpec (reverse body') tlab

perBlockInfer mode marker mi b = do
    addUserSpecsToTracked b
    -- Go inside child blocks
    descendReverseM (descendBiReverseM (perBlockInfer mode marker mi)) b


addUserSpecsToTracked :: F.Block (FA.Analysis A) -> Inferer ()
addUserSpecsToTracked b = do
  let comments = filter isComment $ children b
  mapM_ addCommentSpec comments

addCommentSpec :: F.Block (FA.Analysis A) -> Inferer ()
addCommentSpec b@(F.BlComment ann srcSpan _) = do
  -- If we have a comment that is actually a specification then record that
  -- this has been assigned so that we don't generate extra specifications
  -- that overlap with user-given ones
  let ann' = FA.prevAnnotation ann
  -- Check if we have a spec
  case (stencilSpec ann', stencilBlock ann') of
    -- Comment contains an (uncoverted) specification and an associated block
    (Just (Left (Gram.SpecDec spec vars)), Just block) -> do
      -- Is the block an assignment
      let F.BlStatement _ span _ F.StExpressionAssign{} = block
      -- Then update the list of spans+vars that have specifications
      sequence_ $ fmap (addGSpec spec span) vars
    _ -> pure ()
  pure ()

getInductionVar :: Maybe (F.DoSpecification (FA.Analysis A)) -> [Variable]
getInductionVar (Just (F.DoSpecification _ _ (F.StExpressionAssign _ _ e _) _ _))
  | isVariableExpr e = [FA.varName e]
getInductionVar _ = []

isStencilDo :: F.Block (FA.Analysis A) -> Bool
isStencilDo (F.BlDo _ _ _ _ _ mDoSpec body _) =
 -- Check to see if the body contains any affine use of the induction variable
 -- as a subscript
 case getInductionVar mDoSpec of
    [] -> False
    [ivar] -> not (null exprs) &&
               and [ all (\sub -> sub `isNeighbour` [ivar]) subs' |
               F.ExpSubscript _ _ _ subs <- exprs
               , let subs' = F.aStrip subs
               , not (null subs') ]
      where exprs = universeBi upToNextDo :: [F.Expression (FA.Analysis A)]
            upToNextDo = takeWhile (not . isDo) body
            isDo F.BlDo{} = True
            isDo _            = False
isStencilDo _  = False

{- *** 2 .Conversion from indexing expressions -}

isNeighbour :: Data a => F.Index (FA.Analysis a) -> [Variable] -> Bool
isNeighbour exp vs =
    case ixToNeighbour' vs exp of
        Neighbour _ _ -> True
        _             -> False

--------------------------------------------------

-- Cute <3
-- Penelope's first code, 20/03/2016.
-- iii././//////////////////////. mvnmmmmmmmmmu

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
