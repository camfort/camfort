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

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Camfort.Specification.Stencils.CheckFrontend
  (
    -- * Stencil checking
    CheckResult
  , stencilChecking
  , checkFailure
  , existingStencils
  ) where

import Control.Arrow
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict hiding (Product)
import Data.Generics.Uniplate.Operations
import Data.List (intercalate)

import Camfort.Analysis.Annotations
import Camfort.Analysis.CommentAnnotator
import qualified Camfort.Helpers.Vec as V
import Camfort.Specification.Stencils.CheckBackend
import qualified Camfort.Specification.Stencils.Consistency as C
import Camfort.Specification.Stencils.Generate
import qualified Camfort.Specification.Stencils.Grammar as Gram
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.Syntax

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import qualified Data.Map as M
import Data.Maybe
import Algebra.Lattice (joins1)
import Data.Int
import qualified Data.Set as S

newtype CheckResult = CheckResult { getCheckResult :: [StencilResult] }

-- | Represents only the check results for invalid stencils.
newtype CheckError  = CheckError { getCheckError :: [StencilCheckError] }

-- | Retrieve the checks for invalid stencils from a 'CheckResult'. Result is
-- Nothing if there are no invalid checks.
checkFailure :: CheckResult -> Maybe CheckError
checkFailure c = case catMaybes $ fmap toFailure (getCheckResult c) of
                 [] -> Nothing
                 xs -> Just $ CheckError xs
  where toFailure SCOkay{} = Nothing
        toFailure (SCFail err) = Just err

-- | Result of stencil validation.
data StencilResult
  -- | No issues were identified with the stencil at the given position.
  = SCOkay { scSpan :: FU.SrcSpan
           , scSpec :: Specification
           , scVar  :: Variable
           , scBodySpan :: FU.SrcSpan
           }
  -- | Validation of stencil failed. See 'StencilCheckError' for information
  -- on the types of validation errors that can occur.
  | SCFail StencilCheckError

-- | Represents a way in which validation of a stencil can fail.
data StencilCheckError
  -- | Error occurred during conversion from parsed representation to AST.
  = SynToAstError SynToAstError FU.SrcSpan
  -- | The existing stencil conflicts with an inferred stencil.
  | NotWellSpecified (FU.SrcSpan, SpecDecls) (FU.SrcSpan, SpecDecls)
  -- | The stencil could not be parsed correctly.
  | ParseError String

-- | Pretty print a message with suitable spacing after the source position.
prettyWithSpan :: FU.SrcSpan -> String -> String
prettyWithSpan srcSpan s = show srcSpan ++ "    " ++ s

instance Show CheckResult where
  show = intercalate "\n" . fmap show . getCheckResult

instance Show CheckError where
  show = intercalate "\n" . fmap show . getCheckError

instance Show StencilResult where
  show SCOkay{ scSpan = span } = prettyWithSpan span "Correct."
  show (SCFail err)  = show err

instance Show StencilCheckError where
  show (SynToAstError err srcSpan) = prettyWithSpan srcSpan (show err)
  show (NotWellSpecified (spanActual, stencilActual) (spanInferred, stencilInferred)) =
    let sp = replicate 8 ' '
    in concat [prettyWithSpan spanActual "Not well specified.\n", sp,
               "Specification is:\n", sp, sp, pprintSpecDecls stencilActual, "\n",
               sp, "but at ", show spanInferred, " the code behaves as\n", sp, sp,
               pprintSpecDecls stencilInferred]
  show (ParseError s) = s

-- Entry point
stencilChecking :: F.ProgramFile (FA.Analysis A) -> CheckResult
stencilChecking pf = CheckResult . snd . runWriter $
  do -- Attempt to parse comments to specifications
     pf' <- mapWriter (second (fmap $ SCFail . ParseError)) $ annotateComments Gram.specParser pf

         -- get map of AST-Block-ID ==> corresponding AST-Block
     let bm         = FAD.genBlockMap pf'
         -- get map of program unit  ==> basic block graph
         bbm        = FAB.genBBlockMap pf'
         -- build the supergraph of global dependency
         sgr        = FAB.genSuperBBGr bbm
         -- extract the supergraph itself
         gr         = FAB.superBBGrGraph sgr
         -- get map of variable name ==> { defining AST-Block-IDs }
         dm         = FAD.genDefMap bm
         -- perform reaching definitions analysis
         rd         = FAD.reachingDefinitions dm gr
         -- create graph of definition "flows"
         flowsGraph =  FAD.genFlowsToGraph bm dm gr rd
         -- identify every loop by its back-edge
         beMap      = FAD.genBackEdgeMap (FAD.dominators gr) gr
         ivmap      = FAD.genInductionVarMapByASTBlock beMap gr
         results    = descendBiM perProgramUnitCheck pf'

     let (_, output) = flip evalState (([], Nothing), ivmap)
                     . flip runReaderT flowsGraph
                     . runWriterT $ results
     tell output

type Checker a =
  WriterT [StencilResult]
      (ReaderT (FAD.FlowsGraph A)
            (State ((RegionEnv, Maybe F.ProgramUnitName), FAD.InductionVarMapByASTBlock))) a

-- If the annotation contains an unconverted stencil specification syntax tree
-- then convert it and return an updated annotation containing the AST
parseCommentToAST :: FA.Analysis A -> FU.SrcSpan -> Checker (Either SynToAstError (FA.Analysis A))
parseCommentToAST ann span =
  case stencilSpec (FA.prevAnnotation ann) of
    Just (Left stencilComment) -> do
         ((regionEnv, _), _) <- get
         let ?renv = regionEnv
          in case synToAst stencilComment of
               Right ast  -> pure . pure $ onPrev
                              (\ann -> ann {stencilSpec = Just (Right ast)}) ann
               Left err   -> pure . Left $ err

    _ -> pure . pure $ ann

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
  ast       <- parseCommentToAST ann span
  case ast of
    Left err -> tell [SCFail $ SynToAstError err span] *> pure b
    Right ann' -> do
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
                checkStencil flowsGraph s specDecls span' subs span
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

-- | Validate the stencil and log an appropriate result.
checkStencil :: FAD.FlowsGraph A -> F.Block (FA.Analysis A) -> SpecDecls
  -> FU.SrcSpan -> [F.Index (FA.Analysis Annotation)] -> FU.SrcSpan -> Checker ()
checkStencil flowsGraph s specDecls spanInferred subs span = do
  -- Create list of relative indices
  ivmap <- snd <$> get
  -- Do analysis
  let lhsN         = fromMaybe [] (neighbourIndex ivmap subs)
      relOffsets = fst . runWriter $ genOffsets flowsGraph ivmap lhsN [s]
      multOffsets = map (\relOffset ->
          case relOffset of
          (var, (True, offsets)) -> (var, Mult offsets)
          (var, (False, offsets)) -> (var, Once offsets)) relOffsets
      expandedDecls =
          concatMap (\(vars,spec) -> map (flip (,) spec) vars) specDecls
  -- Model and compare the current and specified stencil specs
  if checkOffsetsAgainstSpec multOffsets expandedDecls
    then mapM_ (\(v,s) -> tell [SCOkay span s v spanInferred]) expandedDecls
    else do
    let inferred = fst . fst . runWriter $ genSpecifications flowsGraph ivmap lhsN [s]
    tell [SCFail $ NotWellSpecified (span, specDecls) (spanInferred, inferred)]

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

existingStencils :: CheckResult -> [(Specification, FU.SrcSpan, Variable)]
existingStencils = mapMaybe getExistingStencil . getCheckResult
  where getExistingStencil SCFail{}                  = Nothing
        getExistingStencil (SCOkay _ spec var bodySpan) = Just (spec, bodySpan, var)

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
