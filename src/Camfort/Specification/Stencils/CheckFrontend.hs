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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Camfort.Specification.Stencils.CheckFrontend
  (
    -- * Stencil checking
    stencilChecking
    -- ** Validation Results
  , CheckResult
  , checkFailure
  , checkWarnings
    -- ** Helpers
  , existingStencils
  ) where

import Control.Arrow
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict hiding (Product)
import Data.Function (on)
import Data.Generics.Uniplate.Operations
import Data.List (intercalate, sort, union)

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

newtype CheckResult = CheckResult [StencilResult]

-- | Retrieve a list of 'StencilResult' from a 'CheckResult'.
--
-- Ensures correct ordering of results.
getCheckResult :: CheckResult -> [StencilResult]
getCheckResult (CheckResult rs) = sort rs

instance Eq CheckResult where
  (==) = (==) `on` getCheckResult

-- | Represents only the check results for invalid stencils.
newtype CheckError  = CheckError { getCheckError :: [StencilCheckError] }

-- | Represents only the check results that resulted in warnings.
newtype CheckWarning = CheckWarning { getCheckWarning :: [StencilCheckWarning] }

-- | Retrieve the checks for invalid stencils from a 'CheckResult'. Result is
-- Nothing if there are no invalid checks.
checkFailure :: CheckResult -> Maybe CheckError
checkFailure c = case catMaybes $ fmap toFailure (getCheckResult c) of
                 [] -> Nothing
                 xs -> Just $ CheckError xs
  where toFailure (SCFail err) = Just err
        toFailure _            = Nothing

checkWarnings :: CheckResult -> Maybe CheckWarning
checkWarnings c = case catMaybes $ fmap toWarning (getCheckResult c) of
                    [] -> Nothing
                    xs -> Just $ CheckWarning xs
  where toWarning (SCWarn warn) = Just warn
        toWarning _             = Nothing

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
  -- | A warning which shouldn't interrupt other procedures.
  | SCWarn StencilCheckWarning
  deriving (Eq)

class GetSpan a where
  getSpan :: a -> FU.SrcSpan

instance GetSpan StencilResult where
  getSpan SCOkay{scSpan = srcSpan} = srcSpan
  getSpan (SCFail err)             = getSpan err
  getSpan (SCWarn warn)            = getSpan warn

instance GetSpan StencilCheckError where
  getSpan (SynToAstError     _ srcSpan)      = srcSpan
  getSpan (NotWellSpecified  (srcSpan, _) _) = srcSpan
  getSpan (ParseError srcSpan _)             = srcSpan
  getSpan (RegionExists srcSpan _)           = srcSpan

instance GetSpan StencilCheckWarning where
  getSpan (DuplicateSpecification srcSpan) = srcSpan
  getSpan (UnusedRegion srcSpan _)         = srcSpan

instance Ord StencilResult where
  compare = compare `on` getSpan

instance Ord StencilCheckError where
  compare = compare `on` getSpan

-- | Represents a way in which validation of a stencil can fail.
data StencilCheckError
  -- | Error occurred during conversion from parsed representation to AST.
  = SynToAstError SynToAstError FU.SrcSpan
  -- | The existing stencil conflicts with an inferred stencil.
  | NotWellSpecified (FU.SrcSpan, SpecDecls) (FU.SrcSpan, SpecDecls)
  -- | The stencil could not be parsed correctly.
  | ParseError FU.SrcSpan String
  -- | A definition for the region alias already exists.
  | RegionExists FU.SrcSpan Variable
  deriving (Eq)

-- | Create a check result informating a user of a 'SynToAstError'.
synToAstError :: SynToAstError -> FU.SrcSpan -> StencilResult
synToAstError err srcSpan = SCFail $ SynToAstError err srcSpan

-- | Create a check result informating a user of a 'NotWellSpecified' error.
notWellSpecified :: (FU.SrcSpan, SpecDecls) -> (FU.SrcSpan, SpecDecls) -> StencilResult
notWellSpecified got inferred = SCFail $ NotWellSpecified got inferred

-- | Create a check result informating a user of a parse error.
parseError :: FU.SrcSpan -> String -> StencilResult
parseError srcSpan err = SCFail $ ParseError srcSpan err

-- | Create a check result informating that a region already exists.
regionExistsError :: FU.SrcSpan -> Variable -> StencilResult
regionExistsError srcSpan r = SCFail $ RegionExists srcSpan r

-- | Represents a non-fatal validation warning.
data StencilCheckWarning
  -- | Specification is defined multiple times.
  = DuplicateSpecification FU.SrcSpan
  -- | Region is defined but not used.
  | UnusedRegion FU.SrcSpan Variable
  deriving (Eq)

-- | Create a check result informing a user of a duplicate specification.
duplicateSpecification :: FU.SrcSpan -> StencilResult
duplicateSpecification = SCWarn . DuplicateSpecification

-- | Create a check result informating an unused region.
unusedRegion :: FU.SrcSpan -> Variable -> StencilResult
unusedRegion srcSpan var = SCWarn $ UnusedRegion srcSpan var

specOkay :: FU.SrcSpan -> Specification -> Variable -> FU.SrcSpan -> StencilResult
specOkay spanSpec@(FU.SrcSpan (FU.Position o1 _ _) (FU.Position o2 _ _)) spec var spanBody@(FU.SrcSpan (FU.Position o1' _ _) (FU.Position o2' _ _)) =
  SCOkay { scSpan      = spanSpec
         , scSpec      = spec
         , scBodySpan  = spanBody
         , scVar       = var
         }

-- | Pretty print a message with suitable spacing after the source position.
prettyWithSpan :: FU.SrcSpan -> String -> String
prettyWithSpan srcSpan s = show srcSpan ++ "    " ++ s

instance Show CheckResult where
  show = intercalate "\n" . fmap show . getCheckResult

instance Show CheckError where
  show = intercalate "\n" . fmap show . getCheckError

instance Show CheckWarning where
  show = intercalate "\n" . fmap show . getCheckWarning

instance Show StencilResult where
  show SCOkay{ scSpan = span } = prettyWithSpan span "Correct."
  show (SCFail err)            = show err
  show (SCWarn warn)           = show warn

instance Show StencilCheckError where
  show (SynToAstError err srcSpan) = prettyWithSpan srcSpan (show err)
  show (NotWellSpecified (spanActual, stencilActual) (spanInferred, stencilInferred)) =
    let sp = replicate 8 ' '
    in concat [prettyWithSpan spanActual "Not well specified.\n", sp,
               "Specification is:\n", sp, sp, pprintSpecDecls stencilActual, "\n",
               sp, "but at ", show spanInferred, " the code behaves as\n", sp, sp,
               pprintSpecDecls stencilInferred]
  show (ParseError srcSpan err) = prettyWithSpan srcSpan err
  show (RegionExists srcSpan name) =
    prettyWithSpan srcSpan ("Region '" ++ name ++ "' already defined")

instance Show StencilCheckWarning where
  show (DuplicateSpecification srcSpan) = prettyWithSpan srcSpan
    "Warning: Duplicate specification."
  show (UnusedRegion srcSpan name)      = prettyWithSpan srcSpan $
    "Warning: Unused region '" ++ name ++ "'"

-- Entry point
stencilChecking :: F.ProgramFile (FA.Analysis A) -> CheckResult
stencilChecking pf = CheckResult . snd . runWriter $
  do -- Attempt to parse comments to specifications
     pf' <- mapWriter (second $ fmap (uncurry parseError)) $ annotateComments Gram.specParser pf

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

     let addUnusedRegionsToResult = do
           regions'     <- fmap regions get
           usedRegions' <- fmap usedRegions get
           let unused = filter ((`notElem` usedRegions') . snd) regions'
           mapM_ (addResult . uncurry unusedRegion) unused
         output = checkResult $ execState
           (runReaderT
             (runChecker (results >> addUnusedRegionsToResult))
             flowsGraph)
           (startState ivmap)

     tell output

data CheckState = CheckState
  { regionEnv     :: RegionEnv
  , checkResult   :: [StencilResult]
  , prog          :: Maybe F.ProgramUnitName
  , ivMap         :: FAD.InductionVarMapByASTBlock
  , regions       :: [(FU.SrcSpan, Variable)]
  , usedRegions   :: [Variable]
  }

addResult :: StencilResult -> Checker ()
addResult r = modify (\s -> s { checkResult = r : checkResult s })

-- | Remove the given regions variables from the tracked unused regions.
informRegionsUsed :: [Variable] -> Checker ()
informRegionsUsed regions = modify
  (\s -> s { usedRegions = usedRegions s `union` regions })

-- | Start tracking a region.
addRegionToTracked :: FU.SrcSpan -> Variable -> Checker ()
addRegionToTracked srcSpan@(FU.SrcSpan (FU.Position o1 _ _) (FU.Position o2 _ _)) r =
  modify (\s -> s { regions = (srcSpan, r) : regions s })

-- | True if the region name is already tracked.
regionExists :: Variable -> Checker Bool
regionExists reg = do
  knownNames <- fmap (fmap snd . regions) get
  pure $ reg `elem` knownNames

startState :: FAD.InductionVarMapByASTBlock -> CheckState
startState ivmap =
  CheckState { regionEnv     = []
             , checkResult   = []
             , prog          = Nothing
             , ivMap         = ivmap
             , regions       = []
             , usedRegions   = []
             }

newtype Checker a =
  Checker { runChecker :: ReaderT (FAD.FlowsGraph A) (State CheckState) a }
  deriving ( Functor, Applicative, Monad
           , MonadReader (FAD.FlowsGraph A)
           , MonadState CheckState
           )

-- If the annotation contains an unconverted stencil specification syntax tree
-- then convert it and return an updated annotation containing the AST
parseCommentToAST :: FA.Analysis A -> FU.SrcSpan -> Checker (Either SynToAstError (FA.Analysis A))
parseCommentToAST ann span =
  case getParseSpec (FA.prevAnnotation ann) of
    Just stencilComment -> do
         informRegionsUsed (Gram.reqRegions stencilComment)
         renv <- fmap regionEnv get
         let ?renv = renv
         case synToAst stencilComment of
           Right ast -> do
             pfun <- either (\reg@(var,_) -> do
                        exists <- regionExists var
                        if exists
                          then addResult (regionExistsError span var)
                               >> pure id
                          else addRegionToTracked span var
                               >> pure (giveRegionSpec reg))
                     (pure . giveAstSpec . pure) ast
             pure . pure $ onPrev pfun ann
           Left err  -> pure . Left $ err

    _ -> pure . pure $ ann

-- If the annotation contains an encapsulated region environment, extract it
-- and add it to current region environment in scope
updateRegionEnv :: FA.Analysis A -> Checker ()
updateRegionEnv ann =
  case getRegionSpec (FA.prevAnnotation ann) of
    Just renv -> modify (\s -> s { regionEnv = renv : regionEnv s })
    _         -> pure ()

checkOffsetsAgainstSpec :: [(Variable, Multiplicity [[Int]])]
                        -> [(Variable, Specification)]
                        -> Bool
checkOffsetsAgainstSpec offsetMaps specMaps =
  variablesConsistent &&
  all (\case
          (spec, Once (V.VL vs)) -> spec `C.consistent` (Once . toUNF) vs == C.Consistent
          (spec, Mult (V.VL vs)) -> spec `C.consistent` (Mult . toUNF) vs == C.Consistent)
  specToVecList
  where
    variablesConsistent =
      let vs1 = sort . fmap fst $ offsetMaps
          vs2 = sort . fmap fst $ specMaps
      in vs1 == vs2
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
    modify (\s -> s { prog = Just $ FA.puName p })
    descendBiM perBlockCheck p
perProgramUnitCheck p = descendBiM perBlockCheck p

perBlockCheck :: F.Block (FA.Analysis A) -> Checker (F.Block (FA.Analysis A))

perBlockCheck b@(F.BlComment ann span _) = do
  ast       <- parseCommentToAST ann span
  case ast of
    Left err -> addResult (synToAstError err span) *> pure b
    Right ann' -> do
      flowsGraph <- ask
      updateRegionEnv ann'
      let b' = F.setAnnotation ann' b
      case (getAstSpec $ FA.prevAnnotation ann', stencilBlock $ FA.prevAnnotation ann') of
        -- Comment contains a specification and an Associated block
        (Just specDecls, Just block) ->
         case block of
          s@(F.BlStatement _ span' _ (F.StExpressionAssign _ _ lhs _)) -> do
             checkStencil flowsGraph s specDecls span' (isArraySubscript lhs) span
             return b'

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
  -> FU.SrcSpan -> Maybe [F.Index (FA.Analysis Annotation)] -> FU.SrcSpan -> Checker ()
checkStencil flowsGraph s specDecls spanInferred maybeSubs span = do
  -- Work out whether this is a stencil (non empty LHS indices) or not
  let (subs, isStencil) = case maybeSubs of
                             Nothing -> ([], False)
                             Just subs -> (subs, True)
  -- Create list of relative indices
  ivmap <- fmap ivMap get
  -- Do analysis
  let lhsN         = fromMaybe [] (neighbourIndex ivmap subs)
      relOffsets = fst . runWriter $ genOffsets flowsGraph ivmap lhsN [s]
      multOffsets = map (\relOffset ->
          case relOffset of
          (var, (True, offsets)) -> (var, Mult offsets)
          (var, (False, offsets)) -> (var, Once offsets)) relOffsets
      expandedDecls =
          concatMap (\(vars,spec) -> map (flip (,) spec) vars) specDecls

  let userDefinedIsStencils = map (\(_, Specification _ b) -> b) specDecls
  -- Model and compare the current and specified stencil specs
  if (all (isStencil ==) userDefinedIsStencils) && checkOffsetsAgainstSpec multOffsets expandedDecls
    then mapM_ (\spec@(v,s) -> do
                   specExists <- seenBefore spec
                   if specExists then addResult (duplicateSpecification span)
                     else addResult (specOkay span s v spanInferred)) expandedDecls
    else do
    let inferred = fst . fst . runWriter $ genSpecifications flowsGraph ivmap lhsN s
    addResult (notWellSpecified (span, specDecls) (spanInferred, inferred))
  where
    seenBefore :: (Variable, Specification) -> Checker Bool
    seenBefore (v,spec) = do
          checkLog <- fmap checkResult get
          pure $ any (\x -> case x of
                              SCOkay{ scSpec=spec'
                                    , scBodySpan=bspan
                                    , scVar = var}
                                -> spec' == spec && bspan == spanInferred && v == var
                              _ -> False) checkLog

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
  where getExistingStencil (SCOkay _ spec var bodySpan) = Just (spec, bodySpan, var)
        getExistingStencil _                            = Nothing

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
