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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}

module Camfort.Specification.Stencils.CheckFrontend where

import Data.Generics.Uniplate.Operations
import Control.Arrow
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict hiding (Product)

import Camfort.Specification.Stencils.CheckBackend
import qualified Camfort.Specification.Stencils.Grammar as Gram
import Camfort.Specification.Stencils.Annotation
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.InferenceFrontend hiding (LogLine)
import Camfort.Specification.Stencils.InferenceBackend
import Camfort.Specification.Stencils.Synthesis
import Camfort.Specification.Stencils.Syntax
import Camfort.Analysis.Annotations
import Camfort.Analysis.CommentAnnotator
import Camfort.Helpers

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import qualified Data.Map as M
import Data.Maybe
import Data.List

-- Entry point
stencilChecking :: FAR.NameMap -> F.ProgramFile (FA.Analysis A) -> [String]
stencilChecking nameMap pf = snd . runWriter $
  do -- Attempt to parse comments to specifications
     pf' <- annotateComments Gram.specParser pf

     -- get map of AST-Block-ID ==> corresponding AST-Block
     let bm    = FAD.genBlockMap pf'
     -- get map of program unit ==> basic block graph
     let bbm   = FAB.genBBlockMap pf'
     -- build the supergraph of global dependency
     let sgr   = FAB.genSuperBBGr bbm
     -- extract the supergraph itself
     let gr    = FAB.superBBGrGraph sgr
     -- get map of variable name ==> { defining AST-Block-IDs }
     let dm    = FAD.genDefMap bm
     let pprint = map (\(span, spec) -> show span ++ "\t" ++ spec)
     -- perform reaching definitions analysis
     let rd    = FAD.reachingDefinitions dm gr
     -- create graph of definition "flows"
     let flTo =  FAD.genFlowsToGraph bm dm gr rd
     -- identify every loop by its back-edge
     let beMap = FAD.genBackEdgeMap (FAD.dominators gr) gr
     let ivmap = FAD.genInductionVarMapByASTBlock beMap gr
     let results = let ?flowsGraph = flTo in
                    let ?nameMap = nameMap
                      in descendBiM perProgramUnitCheck pf'
     -- Format output
     let a@(_, output) = evalState (runWriterT results) (([], Nothing), ivmap)
     tell $ pprint output

type LogLine = (FU.SrcSpan, String)
type Checker a =
    WriterT [LogLine]
            (State ((RegionEnv, Maybe F.ProgramUnitName), FAD.InductionVarMapByASTBlock)) a

-- If the annotation contains an unconverted stencil specification syntax tree
-- then convert it and return an updated annotation containing the AST
parseCommentToAST :: FA.Analysis A -> FU.SrcSpan -> Checker (FA.Analysis A)
parseCommentToAST ann span =
  case stencilSpec (FA.prevAnnotation ann) of
    Just (Left stencilComment) -> do
         ((regionEnv, _), _) <- get
         let ?renv = regionEnv
          in case synToAst stencilComment of
               Left err   -> error $ show span ++ ": " ++ err
               Right ast  -> return $ onPrev
                              (\ann -> ann {stencilSpec = Just (Right ast)}) ann
    _ -> return ann

-- If the annotation contains an encapsulated region environment, extract it
-- and add it to current region environment in scope
updateRegionEnv :: FA.Analysis A -> Checker ()
updateRegionEnv ann =
  case stencilSpec (FA.prevAnnotation ann) of
    Just (Right (Left regionEnv)) -> modify $ first (first (regionEnv ++))
    _                             -> return ()

checkOffsetsAgainstSpec :: [(Variable, Multiplicity [[Int]])]
                        -> [(Variable, Specification)]
                        -> Bool
checkOffsetsAgainstSpec offsetMaps =
    all (\(var1, Specification mult)->
      all (\(var2, offsets) ->
        var1 /= var2 || noAllInfinity offsets `consistent` mult) offsetMaps)
    where
      noAllInfinity (Single a) =
        Single $ filter (not . all (== absoluteRep)) a
      noAllInfinity (Multiple a) =
        Multiple $ filter (not . all (== absoluteRep)) a

-- Go into the program units first and record the module name when
-- entering into a module
perProgramUnitCheck :: (?nameMap :: FAR.NameMap, ?flowsGraph :: FAD.FlowsGraph A)
   => F.ProgramUnit (FA.Analysis A) -> Checker (F.ProgramUnit (FA.Analysis A))
perProgramUnitCheck p@F.PUModule{} = do
    modify $ first (second (const (Just $ FA.puName p)))
    descendBiM perBlockCheck p
perProgramUnitCheck p = descendBiM perBlockCheck p

perBlockCheck :: (?nameMap :: FAR.NameMap, ?flowsGraph :: FAD.FlowsGraph A)
   =>  F.Block (FA.Analysis A) -> Checker (F.Block (FA.Analysis A))

perBlockCheck b@(F.BlComment ann span _) = do
  ann' <- parseCommentToAST ann span
  updateRegionEnv ann'
  let b' = F.setAnnotation ann' b
  case (stencilSpec $ FA.prevAnnotation ann', stencilBlock $ FA.prevAnnotation ann') of
    -- Comment contains a specification and an Associated block
    (Just (Right (Right specDecls)), Just block) ->
     case block of
      s@(F.BlStatement ann span' _ (F.StExpressionAssign _ _ lhs rhs)) ->
       case isArraySubscript lhs of
         Just subs -> do
            -- Create list of relative indices
            ivmap <- snd <$> get
            -- Do analysis
            let realName v   = v `fromMaybe` (v `M.lookup` ?nameMap)
            let lhsN         = fromMaybe [] (neighbourIndex ivmap subs)
            let correctNames = map (first realName)
            let relOffsets = correctNames . fst . runWriter $ genOffsets ivmap lhsN [s]
            let multOffsets = map (\relOffset ->
                  case relOffset of
                    (var, (True, offsets)) -> (var, Multiple offsets)
                    (var, (False, offsets)) -> (var, Single offsets)) relOffsets
            let expandedDecls =
                  concatMap (\(vars,spec) -> map (flip (,) spec) vars) specDecls
            -- Model and compare the current and specified stencil specs
            if checkOffsetsAgainstSpec multOffsets expandedDecls
              then tell [ (span, "Correct.") ]
              else do
                let correctNames2 =  map (first (map realName))
                let inferred = correctNames2 . fst . runWriter $ genSpecifications ivmap lhsN [s]
                tell [ (span, "Not well specified.\n"
                              ++ "\tSpecification is:\t "
                              ++ pprintSpecDecls specDecls
                              ++ "\tbut at " ++ show span' ++ " the code behaves as"
                              ++ "\n\t                 \t "
                              ++ pprintSpecDecls inferred) ]
            return b'
         Nothing -> return b'

      (F.BlDo ann span _ _ _ mDoSpec body _) ->
           -- Stub, maybe collect stencils inside 'do' block
           return b'
      _ -> return b'
    _ -> return b'

perBlockCheck b@(F.BlDo ann span _ _ _ mDoSpec body _) = do
   -- descend into the body of the do-statement
   mapM_ (descendBiM perBlockCheck) body
   -- Remove any induction variable from the state
   return b

perBlockCheck b = do
  updateRegionEnv . F.getAnnotation $ b
  -- Go inside child blocks
  mapM_ (descendBiM perBlockCheck) $ children b
  return b

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
