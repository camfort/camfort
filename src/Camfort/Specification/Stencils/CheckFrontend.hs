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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}

module Camfort.Specification.Stencils.CheckFrontend where

import Data.Generics.Uniplate.Operations
import Control.Arrow
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict hiding (Product)

import qualified Camfort.Helpers.Vec as V
import Camfort.Specification.Stencils.CheckBackend
import qualified Camfort.Specification.Stencils.Consistency as C
import qualified Camfort.Specification.Stencils.Grammar as Gram
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.InferenceFrontend hiding (LogLine)
import Camfort.Specification.Stencils.Syntax
import Camfort.Analysis.Annotations
import Camfort.Analysis.CommentAnnotator

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import qualified Data.Map as M
import Data.Maybe
import Algebra.Lattice (joins1)
import Data.Int
import qualified Data.Set as S

-- Entry point
stencilChecking :: F.ProgramFile (FA.Analysis A) -> [String]
stencilChecking pf = snd . runWriter $
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
     let pprint = map (\(span, spec) -> show span ++ "    " ++ spec)
     -- perform reaching definitions analysis
     let rd    = FAD.reachingDefinitions dm gr
     -- create graph of definition "flows"
     let flowsGraph =  FAD.genFlowsToGraph bm dm gr rd
     -- identify every loop by its back-edge
     let beMap = FAD.genBackEdgeMap (FAD.dominators gr) gr
     let ivmap = FAD.genInductionVarMapByASTBlock beMap gr
     let results = descendBiM perProgramUnitCheck pf'

     -- Format output
     let (_, output) = flip evalState (([], Nothing), ivmap)
                     . flip runReaderT flowsGraph
                     . runWriterT
                     $ results
     tell $ pprint output

type LogLine = (FU.SrcSpan, String)
type Checker a =
    WriterT [LogLine]
      (ReaderT (FAD.FlowsGraph A)
            (State ((RegionEnv, Maybe F.ProgramUnitName), FAD.InductionVarMapByASTBlock))) a

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
checkOffsetsAgainstSpec offsetMaps specMaps =
    flip all specToVecList $
      \case
        (spec, Once (V.VL vs)) -> spec `C.consistent` (Once . toUNF) vs == C.Consistent
        (spec, Mult (V.VL vs)) -> spec `C.consistent` (Mult . toUNF) vs == C.Consistent
  where
    toUNF :: [ V.Vec n Int64 ] -> UnionNF n Offsets
    toUNF = joins1 . map (return . fmap intToSubscript)

    -- This function generates the special offsets subspace, subscript,
    -- that either had one element or is the whole set.
    intToSubscript :: Int64 -> Offsets
    intToSubscript i
      | fromIntegral i == absoluteRep = SetOfIntegers
      | otherwise = Offsets . S.singleton $ i

    -- Convert list of list of indices into vectors and wrap them around
    -- existential so that we don't have to prove they are all of the same
    -- size.
    specToVecList :: [ (Specification, Multiplicity (V.VecList Int64)) ]
    specToVecList = map (second (fmap V.fromLists)) specToIxs

    specToIxs :: [ (Specification, Multiplicity [ [ Int64 ] ]) ]
    specToIxs = pairWithFst specMaps (map (second toInt64) offsetMaps)

    toInt64 :: Multiplicity [ [ Int ] ] -> Multiplicity [ [ Int64 ] ]
    toInt64 = fmap (map (map fromIntegral))

    -- Given two maps for each key in the first map generate a set of
    -- tuples matching the (val,val') where val and val' are corresponding
    -- values from each set.
    pairWithFst :: Eq a => [ (a, b) ] -> [ (a, c) ] -> [ (b, c) ]
    pairWithFst [] _ = []
    pairWithFst ((key, val):xs) ys =
      map ((val,) . snd) (filter ((key ==) . fst) ys) ++ pairWithFst xs ys

-- Go into the program units first and record the module name when
-- entering into a module
perProgramUnitCheck ::
   F.ProgramUnit (FA.Analysis A) -> Checker (F.ProgramUnit (FA.Analysis A))

perProgramUnitCheck p@F.PUModule{} = do
    modify $ first (second (const (Just $ FA.puName p)))
    descendBiM perBlockCheck p
perProgramUnitCheck p = descendBiM perBlockCheck p

perBlockCheck :: F.Block (FA.Analysis A) -> Checker (F.Block (FA.Analysis A))

perBlockCheck b@(F.BlComment ann span _) = do
  ann'       <- parseCommentToAST ann span
  flowsGraph <- ask
  updateRegionEnv ann'
  let b' = F.setAnnotation ann' b
  case (stencilSpec $ FA.prevAnnotation ann', stencilBlock $ FA.prevAnnotation ann') of
    -- Comment contains a specification and an Associated block
    (Just (Right (Right specDecls)), Just block) ->
     case block of
      s@(F.BlStatement _ span' _ (F.StExpressionAssign _ _ lhs _)) ->
       case isArraySubscript lhs of
         Just subs -> do
            -- Create list of relative indices
            ivmap <- snd <$> get
            -- Do analysis
            let lhsN         = fromMaybe [] (neighbourIndex ivmap subs)
            let relOffsets = fst . runWriter $ genOffsets flowsGraph ivmap lhsN [s]
            let multOffsets = map (\relOffset ->
                  case relOffset of
                    (var, (True, offsets)) -> (var, Mult offsets)
                    (var, (False, offsets)) -> (var, Once offsets)) relOffsets
            let expandedDecls =
                  concatMap (\(vars,spec) -> map (flip (,) spec) vars) specDecls
            -- Model and compare the current and specified stencil specs
            if checkOffsetsAgainstSpec multOffsets expandedDecls
              then tell [ (span, "Correct.") ]
              else do

                let inferred = fst . fst . runWriter $ genSpecifications flowsGraph ivmap lhsN [s]
                let sp = replicate 8 ' '
                tell [ (span,
                     "Not well specified.\n"
                  ++ sp ++ "Specification is:\n"
                  ++ sp ++ sp ++ pprintSpecDecls specDecls ++ "\n"
                  ++ sp ++ "but at " ++ show span' ++ " the code behaves as\n"
                  ++ sp ++ sp ++ pprintSpecDecls inferred) ]

            return b'
         Nothing -> return b'

      -- Stub, maybe collect stencils inside 'do' block
      F.BlDo{} -> return b'
      _ -> return b'
    _ -> return b'

perBlockCheck b@(F.BlDo _ _ _ _ _ _ body _) = do
   -- descend into the body of the do-statement
   mapM_ (descendBiM perBlockCheck) body
   -- Remove any induction variable from the state
   return b

perBlockCheck b = do
  updateRegionEnv . F.getAnnotation $ b
  -- Go inside child blocks
  mapM_ (descendBiM perBlockCheck) $ children b
  return b

genOffsets ::
     FAD.FlowsGraph A
  -> FAD.InductionVarMapByASTBlock
  -> [Neighbour]
  -> [F.Block (FA.Analysis A)]
  -> Writer EvalLog [(Variable, (Bool, [[Int]]))]
genOffsets flowsGraph ivs lhs blocks = do
    let (subscripts, _) = genSubscripts flowsGraph blocks
    assocsSequence $ mkOffsets subscripts
  where
    mkOffsets = M.mapWithKey (\v -> indicesToRelativisedOffsets ivs v lhs)


-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
