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

module Camfort.Analysis.StencilSpecification.CheckFrontend where

import Language.Fortran hiding (Spec)

import Data.Data
import Data.Generics.Uniplate.Operations
import Control.Arrow
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Camfort.Analysis.StencilSpecification.CheckBackend
import qualified Camfort.Analysis.StencilSpecification.Grammar as Gram
import Camfort.Analysis.StencilSpecification.Model
import Camfort.Analysis.StencilSpecification.InferenceFrontend
import Camfort.Analysis.StencilSpecification.InferenceBackend
import Camfort.Analysis.StencilSpecification.Synthesis
import Camfort.Analysis.StencilSpecification.Syntax
import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations
import Camfort.Analysis.CommentAnnotator

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import Data.Graph.Inductive.Graph hiding (isEmpty)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List

instance ASTEmbeddable Annotation Gram.Specification where
  annotateWithAST ann ast = ann { stencilSpec = Just $ Left ast }

instance Linkable Annotation where
  link ann (b@(F.BlDo {})) =
      ann { stencilBlock = Just b }
  link ann (b@(F.BlStatement _ _ _ (F.StExpressionAssign {}))) =
      ann { stencilBlock = Just b }
  link ann b = ann

-- If the annotation contains an unconverted stencil specification syntax tree
-- then convert it and return an updated annotation containing the AST
parseCommentToAST :: Annotation -> FU.SrcSpan ->
  WriterT [(FU.SrcSpan, String)] (State (RegionEnv, [Variable])) Annotation
parseCommentToAST ann span =
  case stencilSpec ann of
    Just (Left stencilComment) -> do
         (regionEnv, _) <- get
         let ?renv = regionEnv
          in case synToAst stencilComment of
               Left err   -> error $ show span ++ ": " ++ err
               Right ast  -> return $ ann { stencilSpec = Just (Right ast) }
    _ -> return ann

-- If the annotation contains an encapsulated region environment, extract it
-- and add it to current region environment in scope
updateRegionEnv :: Annotation -> WriterT [(FU.SrcSpan, String)]
        (State (RegionEnv, [Variable])) ()
updateRegionEnv ann =
  case stencilSpec ann of
    Just (Right (Left regionEnv)) -> modify $ ((++) regionEnv) *** id
    _                             -> return ()

-- For a list of names -> specs, compare the model for each
-- against any matching names in the spec env (second param)
compareModel :: [([F.Name], Specification)] -> SpecDecls -> Bool
compareModel [] _ = True
compareModel ((names, spec) : ss) env =
  foldr (\n r -> compareModel' n spec env && r) True names
  && compareModel ss env
   where compareModel' :: F.Name -> Specification -> SpecDecls -> Bool
         compareModel' name spec1 senv =
          case lookupSpecDecls senv name of
            Just spec2 -> let d1 = dimensionality spec1
                              d2 = dimensionality spec2
                          in let ?dimensionality = d1 `max` d2
                             in mkModel spec1 == mkModel spec2
            Nothing    -> True

perBlockCheck :: F.Block Annotation
   -> WriterT [(FU.SrcSpan, String)]
        (State (RegionEnv, [Variable])) (F.Block Annotation)

perBlockCheck b@(F.BlComment ann span _) = do
  ann' <- parseCommentToAST ann span
  updateRegionEnv ann'
  let b' = F.setAnnotation ann' b
  case (stencilSpec ann', stencilBlock ann') of
    -- Comment contains a specification and an associated block
    (Just (Right (Right specDecls)), Just block) ->
     case block of
      s@(F.BlStatement ann span _ (F.StExpressionAssign _ _ _ rhs)) -> do
        -- Get array indexing (on the RHS)
        let rhsExprs = universeBi rhs :: [F.Expression Annotation]
        let arrayAccesses = collect [
              (v, e) | F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable v)) subs <- rhsExprs
                     , let e = F.aStrip subs
                     , not (null e)]
        -- Create list of relative indices
        (_, ivs) <- get
        let analysis = groupKeyBy
                     . M.toList
                     . M.mapMaybe (ixCollectionToSpec ivs) $ arrayAccesses
        -- Model and compare the current and specified stencil specs
        if compareModel analysis specDecls
           -- Not well-specified
         then tell [ (span, "Correct.") ]
         else tell [ (span, "Not well specified:\n\t\t  expecting: "
                         ++ pprintSpecDecls specDecls
                         ++ "\t\t  actual:    " ++ pprintSpecDecls analysis) ]
        return $ b'
      _ -> return $ b'

      (F.BlDo ann span _ mDoSpec body) -> do
        -- Stub, collect stencils inside 'do' block
        return $ b'
      _ -> return $ b'
    _ -> return b'

perBlockCheck b@(F.BlDo ann span _ mDoSpec body) = do
   let localIvs = getInductionVar mDoSpec
   -- introduce any induction variables into the induction variable state
   modify $ id *** union localIvs
   -- descend into the body of the do-statement
   mapM_ (descendBiM perBlockCheck) body
   -- Remove any induction variable from the state
   modify $ id *** (\\ localIvs)
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