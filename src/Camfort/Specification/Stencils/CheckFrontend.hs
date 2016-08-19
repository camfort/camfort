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

import Data.Data
import Data.Generics.Uniplate.Operations
import Control.Arrow
import Control.Monad.State.Strict
import Control.Monad.Reader
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
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import Data.Graph.Inductive.Graph hiding (isEmpty)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Maybe
import Data.List
import Debug.Trace

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
     let a@(_, output) = evalState (runWriterT $ results) (([], Nothing), ivmap)
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
    Just (Right (Left regionEnv)) -> modify $ (((++) regionEnv) *** id) *** id
    _                             -> return ()

-- Given a mapping from variables to inferred specifications
-- an environment of specification delcarations, for each declared
-- specification check if there is a inferred specification that
-- agrees with it, *up-to the model*
compareInferredToDeclared :: [([F.Name], Specification)] -> SpecDecls -> Bool
compareInferredToDeclared inferreds declareds =
   all (\(names, dec) ->
    all (\name ->
      any (\inf -> eqByModel inf dec) (lookupAggregate inferreds name)
       ) names) declareds

-- Go into the program units first and record the module name when
-- entering into a module
perProgramUnitCheck :: (?nameMap :: FAR.NameMap, ?flowsGraph :: FAD.FlowsGraph A)
   => F.ProgramUnit (FA.Analysis A) -> Checker (F.ProgramUnit (FA.Analysis A))
perProgramUnitCheck p@(F.PUModule {}) = do
    modify $ (id *** (const (Just $ FA.puName p))) *** id
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
      s@(F.BlStatement ann span _ (F.StExpressionAssign _ _ lhs rhs)) ->
       case isArraySubscript lhs of
         Just subs -> do
            -- Create list of relative indices
            (_, ivmap) <- get
            -- Do inference
            let realName v   = v `fromMaybe` (v `M.lookup` ?nameMap)
            let lhsN         = maybe [] id (neighbourIndex ivmap subs)
            let correctNames = map (\(names, spec) -> (map realName names, spec))
            let inferred = correctNames . fst . runWriter $ genSpecifications ivmap lhsN [s]
            -- Model and compare the current and specified stencil specs
            if compareInferredToDeclared inferred specDecls
              then tell [ (span, "Correct.") ]
              else tell [ (span, "Not well specified:\n\t\t  expecting: "
                              ++ pprintSpecDecls specDecls
                              ++ "\t\t  inferred:    " ++ pprintSpecDecls inferred) ]
            return $ b'
         Nothing -> return $ b'
      _ -> return $ b'

      (F.BlDo ann span _ _ _ mDoSpec body _) -> do
           -- Stub, maybe collect stencils inside 'do' block
           return $ b'
      _ -> return $ b'
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
