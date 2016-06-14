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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Camfort.Analysis.StencilSpecification.InferenceFrontend where

import Language.Fortran hiding (Spec)

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Camfort.Analysis.StencilSpecification.InferenceBackend
import Camfort.Analysis.StencilSpecification.Syntax
import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations
import Camfort.Helpers.Vec
import Camfort.Input

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Graph.Inductive.Graph hiding (isEmpty)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List

import Debug.Trace

-- Define modes of interaction with the inference
data InferMode =
  DoMode | AssignMode | CombinedMode
  deriving (Eq, Show, Data, Read)

instance Default InferMode where
    defaultValue = AssignMode

-- The inferer returns information as a LogLine
type LogLine = (FU.SrcSpan, [([Variable], Specification)])
-- The core of the inferer works within this monad
type Inferer = WriterT [LogLine] (ReaderT (Cycles, F.ProgramUnitName, TypeEnv A) (State [Variable]))
type Cycles = [(F.Name, F.Name)]


inferFromAST :: InferMode -> F.ProgramFile (FA.Analysis A) -> [LogLine]
inferFromAST mode pf@(F.ProgramFile cm_pus others) =
 concatMap perPU (universeBi cm_pus)
  where
    -- Run inference per program unit, placing the flowsmap in scope
    perPU :: F.ProgramUnit (FA.Analysis A) -> [LogLine]

    perPU pu | Just gr <- FA.bBlocks $ F.getAnnotation pu =
         let
         -- perform reaching definitions analysis
         rd    = FAD.reachingDefinitions dm gr
         -- create graph of definition "flows"
         flTo =  FAD.genFlowsToGraph bm dm gr rd
         -- VarFlowsToMap: A -> { B, C } indicates that A contributes to B, C
         flMap = FAD.genVarFlowsToMap dm flTo
         -- find 2-cycles: A -> B -> A
         cycs2 = [ (n, m) | (n, ns) <- M.toList flMap
                    , m       <- S.toList ns
                    , ms      <- maybeToList $ M.lookup m flMap
                    , n `S.member` ms && n /= m ]
         -- identify every loop by its back-edge
         beMap = FAD.genBackEdgeMap (FAD.dominators gr) gr

         -- get map of AST-Block-ID ==> corresponding AST-Block
         bm    = FAD.genBlockMap pf
         -- get map of program unit ==> basic block graph
         bbm   = FAB.genBBlockMap pf
         -- stitch all of the graphs together into a 'supergraph'
         --sgr   = FAB.genSuperBBGr bbm
         -- extract the supergraph itself
         --gr    = FAB.superBBGrGraph sgr
         -- get map of variable name ==> { defining AST-Block-IDs }
         dm    = FAD.genDefMap bm
         in
          let ?flowsGraph = flTo
          in runInferer cycs2 (F.getName pu) tenv (descendBiM (perBlockInfer mode) pu)
    perPU _ = []


    tenv  = FAT.inferTypes pf

-- | Return list of variable names that flow into themselves via a 2-cycle
findVarFlowCycles :: Data a => F.ProgramFile a -> [(F.Name, F.Name)]
findVarFlowCycles = FAR.underRenaming (findVarFlowCycles' . FAB.analyseBBlocks)
findVarFlowCycles' pf = cycs2
  where
    bm    = FAD.genBlockMap pf     -- get map of AST-Block-ID ==> corresponding AST-Block
    bbm   = FAB.genBBlockMap pf    -- get map of program unit ==> basic block graph
    sgr   = FAB.genSuperBBGr bbm   -- stitch all of the graphs together into a 'supergraph'
    gr    = FAB.superBBGrGraph sgr -- extract the supergraph itself
    dm    = FAD.genDefMap bm       -- get map of variable name ==> { defining AST-Block-IDs }
    rd    = FAD.reachingDefinitions dm gr   -- perform reaching definitions analysis
    flTo  = FAD.genFlowsToGraph bm dm gr rd -- create graph of definition "flows"
    -- VarFlowsToMap: A -> { B, C } indicates that A contributes to B, C.
    flMap = FAD.genVarFlowsToMap dm flTo -- create VarFlowsToMap
    -- find 2-cycles: A -> B -> A
    cycs2 = [ (n, m) | (n, ns) <- M.toList flMap
                     , m       <- S.toList ns
                     , ms      <- maybeToList $ M.lookup m flMap
                     , n `S.member` ms && n /= m ]


{- *** 1 . Core inference over blocks -}

runInferer :: Cycles -> F.ProgramUnitName -> TypeEnv A -> Inferer a -> [LogLine]
runInferer cycles puName tenv =
  flip evalState [] . flip runReaderT (cycles, puName, tenv) . execWriterT


-- Traverse Blocks in the AST and infer stencil specifications
perBlockInfer :: (?flowsGraph :: FAD.FlowsGraph A)
    => InferMode -> F.Block (FA.Analysis A) -> Inferer (F.Block (FA.Analysis A))

perBlockInfer mode b@(F.BlStatement _ span _ (F.StExpressionAssign _ _ lhs _))
  | mode == AssignMode || mode == CombinedMode = do
    ivs <- get
    case lhs of
       F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable v)) subs ->
        -- Left-hand side is a subscript-by translation of an induction variable
        -- or by a range
        if all (isAffineISubscript ivs) (F.aStrip subs)
         then tell [ (span, genSpecifications ivs [b]) ]
         else return ()
       -- Not an assign we are interested in
       _ -> return ()
    return b

perBlockInfer mode b@(F.BlDo _ span _ mDoSpec body) = do
    let localIvs = getInductionVar mDoSpec
    -- introduce any induction variables into the induction variable state
    modify $ union localIvs
    ivs <- get

    if (mode == DoMode || mode == CombinedMode) && isStencilDo b
     then tell [ (span, genSpecifications ivs body) ]
     else return ()

    -- descend into the body of the do-statement
    mapM_ (descendBiM (perBlockInfer mode)) body
    -- Remove any induction variable from the state
    modify $ (\\ localIvs)
    return b

perBlockInfer mode b = do
    -- Go inside child blocks
    mapM_ (descendBiM (perBlockInfer mode)) $ children b
    return b


genSpecifications :: (?flowsGraph :: FAD.FlowsGraph A) =>
   [Variable] -> [F.Block (FA.Analysis A)] -> [([Variable], Specification)]
genSpecifications ivs = groupKeyBy . M.toList . specs
  where specs = M.mapMaybe (relativeIxsToSpec ivs . M.keys)
              . M.unionsWith (M.unionWith (\_ _ -> True))
              . flip evalState []
              . mapM (genSubscripts ivs)

-- Generate all subscripting expressions (that are translations on
-- induction variables) that flow to this block
-- The State monad provides a list of the visited nodes so far
genSubscripts ::
    (?flowsGraph :: FAD.FlowsGraph A) => [Variable] ->
    F.Block (FA.Analysis A) -> State [Int] (M.Map Variable (M.Map [Int] Bool))
genSubscripts ivs block = do
  visited <- get
  case (FA.insLabel $ F.getAnnotation block) of

    Just node ->
     if node `elem` visited
     -- This dependency has already been visited during this traversal
     then return $ M.empty
     -- Fresh dependency
     else do
       put $ node : visited
       let blocksFlowingIn = mapMaybe (lab ?flowsGraph) $ pre ?flowsGraph node
       dependencies <- mapM (genSubscripts ivs) blocksFlowingIn
       let plus = M.unionWith (\_ _ -> True)
       return $ M.unionsWith plus (genRHSsubscripts ivs block : dependencies)

    Nothing -> error $ "Missing a label for: " ++ show block


-- Get all RHS subscript which are translated induction variables
genRHSsubscripts :: [Variable]
                 -> F.Block (FA.Analysis A) -> M.Map Variable (M.Map [Int] Bool)
genRHSsubscripts ivs b =
  convertMultiples . M.map (toListsOfRelativeIndices ivs) $ subscripts
  where
   subscripts :: M.Map Variable [[F.Index (FA.Analysis A)]]
   subscripts = collect [ (v, e)
      | F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable v)) subs <- FA.rhsExprs b
       , let e = F.aStrip subs
       , not (null e) ]

   convertMultiples :: Ord a => M.Map k [a] -> M.Map k (M.Map a Bool)
   -- Marks all 'a' values as 'False' (multiplicity 1) and then if
   -- any duplicate values for 'a' occur, the 'with' functions
   -- marks them as 'True' (multiplicity > 1)
   convertMultiples = M.map (M.fromListWith (\_ _ -> True) . map (,False))

getInductionVar :: Maybe (F.DoSpecification a) -> [Variable]
getInductionVar (Just (F.DoSpecification _ _ (
                        F.StExpressionAssign _ _ (
                          F.ExpValue _ _ (F.ValVariable v)) _) _ _)) = [v]
getInductionVar _ = []


isStencilDo :: F.Block (FA.Analysis A) -> Bool
isStencilDo b@(F.BlDo _ span _ mDoSpec body) =
 -- Check to see if the body contains any affine use of the induction variable
 -- as a subscript
 case getInductionVar mDoSpec of
    [] -> False
    [ivar] -> length exprs > 0 &&
               and [ all (\sub -> sub `isAffineOnVar` ivar) subs' |
               F.ExpSubscript _ _ _ subs <- exprs
               , let subs' = F.aStrip subs
               , not (null subs') ]
      where exprs = universeBi upToNextDo :: [F.Expression (FA.Analysis A)]
            upToNextDo = takeWhile (not . isDo) body
            isDo (F.BlDo {}) = True
            isDo _            = False
isStencilDo _  = False


{- OLD TEMPORAL INFERENCE CODE FROM perBlockInfer on DO
   Temporarily removed

  (cycles, _, _) <- ask
  let lexps = FA.lhsExprs =<< body
  let getTimeSpec e = do
        lhsV <- case e of
          F.ExpValue _ _ (F.ValVariable lhsV) -> Just lhsV
          F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable lhsV)) _ -> Just lhsV
          _ -> Nothing
        v'   <- lookup lhsV cycles
        -- TODO: update with mutual info
        return (lhsV, Specification $ Right $ Dependency [v'] False)

  let tempSpecs = groupKeyBy $ foldl' (\ ts -> maybe ts (:ts) . getTimeSpec) [] lexps

  tell [ (span, tempSpecs) ]
-}

{- *** 2 .Conversion from indexing expressions -}

-- padZeros makes this rectilinear
padZeros :: [[Int]] -> [[Int]]
padZeros ixss = let m = maximum (map length ixss)
                in map (\ixs -> ixs ++ replicate (m - length ixs) 0) ixss

-- Convert list of indexing expressions to a spec
ixCollectionToSpec :: [Variable] -> [[F.Index a]] -> Maybe Specification
ixCollectionToSpec ivs =
    (relativeIxsToSpec ivs) . (toListsOfRelativeIndices ivs)

-- Convert list of relative indices to a spec
relativeIxsToSpec :: [Variable] -> [[Int]] -> Maybe Specification
relativeIxsToSpec ivs ixs =
    if isEmpty exactSpec then Nothing else Just exactSpec
      where exactSpec = inferFromIndices . fromLists $ ixs

toListsOfRelativeIndices :: [Variable] -> [[F.Index a]] -> [[Int]]
toListsOfRelativeIndices ivs = padZeros . map (map (maybe 0 id . (ixToOffset ivs)))

-- Convert indexing expressions that are translations
-- intto their translation offset:
-- e.g., for the expression a(i+1,j-1) then this function gets
-- passed expr = i + 1   (returning +1) and expr = j - 1 (returning -1)
ixToOffset :: [Variable] -> F.Index a -> Maybe Int
-- Range with stride = 1 count as reflexive indexing
ixToOffset ivs (F.IxRange _ _ _ _ Nothing) = Just 0
ixToOffset ivs (F.IxRange _ _ _ _ (Just (F.ExpValue _ _ (F.ValInteger "1")))) =
    Just 0
ixToOffset ivs (F.IxSingle _ _ _ exp) = expToOffset ivs exp
ixToOffset _ _ = Nothing -- If the indexing expression is a range

isAffineOnVar :: F.Index a -> Variable -> Bool
isAffineOnVar exp v = ixToOffset [v] exp /= Nothing

isAffineISubscript :: [Variable] -> F.Index a -> Bool
isAffineISubscript vs exp = ixToOffset vs exp /= Nothing

expToOffset :: [Variable] -> F.Expression a -> Maybe Int
expToOffset ivs (F.ExpValue _ _ (F.ValVariable v))
  | v `elem` ivs = Just 0
  | otherwise    = Just absoluteRep
expToOffset ivs (F.ExpBinary _ _ F.Addition
                                 (F.ExpValue _ _ (F.ValVariable v))
                                 (F.ExpValue _ _ (F.ValInteger offs)))
    | v `elem` ivs = Just $ read offs
expToOffset ivs (F.ExpBinary _ _ F.Addition
                                 (F.ExpValue _ _ (F.ValInteger offs))
                                 (F.ExpValue _ _ (F.ValVariable v)))
    | v `elem` ivs = Just $ read offs
expToOffset ivs (F.ExpBinary _ _ F.Subtraction
                                 (F.ExpValue _ _ (F.ValVariable v))
                                 (F.ExpValue _ _ (F.ValInteger offs)))
   | v `elem` ivs = Just $ if x < 0 then abs x else (- x)
                     where x = read offs
expToOffset ivs _ = Just absoluteRep

--------------------------------------------------


groupKeyBy :: Eq b => [(a, b)] -> [([a], b)]
groupKeyBy = groupKeyBy' . map (\ (k, v) -> ([k], v))

groupKeyBy' []                         = []
groupKeyBy' [(ks, v)]                  = [(ks, v)]
groupKeyBy' ((ks1, v1):((ks2, v2):xs))
  | v1 == v2                           = groupKeyBy' ((ks1 ++ ks2, v1) : xs)
  | otherwise                          = (ks1, v1) : groupKeyBy' ((ks2, v2) : xs)

-- Although type analysis isn't necessary anymore (Forpar does it
-- internally) I'm going to leave this infrastructure in-place in case
-- it might be useful later.
type TypeEnv a = M.Map FAT.TypeScope (M.Map String FA.IDType)
isArrayType :: TypeEnv A -> F.ProgramUnitName -> String -> Bool
isArrayType tenv name v = fromMaybe False $ do
  tmap <- M.lookup (FAT.Local name) tenv `mplus` M.lookup FAT.Global tenv
  idty <- M.lookup v tmap
  cty  <- FA.idCType idty
  return $ cty == FA.CTArray

-- Penelope's first code, 20/03/2016.
-- iii././//////////////////////. mvnmmmmmmmmmu

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End: