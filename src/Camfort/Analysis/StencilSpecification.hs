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

{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleContexts, ImplicitParams, TupleSections #-}

module Camfort.Analysis.StencilSpecification where

import Language.Fortran hiding (Spec)

import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Camfort.Analysis.StencilSpecification.Inference
import Camfort.Analysis.StencilSpecification.Syntax
import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations
import Camfort.Extensions.UnitsForpar (parameterise)
import Camfort.Helpers.Vec
import Camfort.Helpers hiding (lineCol, spanLineCol) -- These two are redefined here for ForPar ASTs

import qualified Forpar.AST as F
import qualified Forpar.Analysis as FA
import qualified Forpar.Analysis.Types as FAT
import qualified Forpar.Analysis.Renaming as FAR
import qualified Forpar.Util.Position as FU

import qualified Data.Map as Map
import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe
import Data.List
import Data.Tuple (swap)
import Data.Ord

import Debug.Trace

--------------------------------------------------
-- For the purposes of development, a representative example is given by running (in ghci):
--      stencilsInf "samples/stencils/one.f" [] () ()

-- Infer and check stencil specifications
infer :: F.ProgramFile Annotation -> String
infer = specInference . FAR.renameAndStrip . FAR.analyseRenames . FA.initAnalysis

check :: Program a -> Program a
check = error "Not yet implemented"

--------------------------------------------------

type LogLine = (FU.SrcSpan, [([Variable], [Specification])])
formatSpec :: FAR.NameMap -> LogLine -> String
formatSpec nm (span, []) = ""
formatSpec nm (span, specs) = loc ++ " \t" ++ (commaSep . nub . map doSpec $ specs) ++ "\n"
  where
    loc                      = show (spanLineCol span)
    commaSep                 = concat . intersperse ", "
    doSpec (arrayVar, spec)  = commaSep (map realName arrayVar) ++ ": " ++ showL (map fixSpec spec)
    realName v               = v `fromMaybe` (v `M.lookup` nm)
    fixSpec (TemporalFwd vs) = TemporalFwd $ map realName vs
    fixSpec (TemporalBwd vs) = TemporalBwd $ map realName vs
    fixSpec s                = s

--------------------------------------------------

-- The inferer works within this monad
type Inferer = WriterT [LogLine] (ReaderT (Cycles, F.ProgramUnitName, TypeEnv A) (State [Variable]))
runInferer :: Cycles -> F.ProgramUnitName -> TypeEnv A -> Inferer a -> [LogLine]
runInferer cycles puName tenv =
  flip evalState [] . flip runReaderT (cycles, puName, tenv) . execWriterT

--------------------------------------------------

-- Provides the main inference procedure for specifications which is
-- currently at the level of per statement for spatial stencils, and
-- per for-loop for temporal stencils
specInference :: (F.ProgramFile A, FAR.NameMap) -> String
specInference (pf, nm) = formatSpec nm =<< logs
  where tenv = FAT.inferTypes pf
        logs = specInference' tenv =<< flowAnalysisArrays pf

specInference' :: TypeEnv A -> (F.ProgramUnit A, FlowsMap) -> [LogLine]
specInference' tenv (pu, flMap) = runInferer cycles (F.getName pu) tenv (descendBiM perBlock pu)
  where cycles = cyclicDependents flMap

--------------------------------------------------

perBlock :: F.Block A -> Inferer (F.Block A)
perBlock b@(F.BlStatement _ span _ (F.StExpressionAssign _ _ _ rhs)) = do
  (_, puName, tenv) <- ask
  -- Get array indexing (on the RHS)
  let rhsExprs = universeBi rhs :: [F.Expression A]
  let arrayAccesses = collect [
          (v, e) | F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValArray _ v)) subs <- rhsExprs
                 , let e = F.aStrip subs
                 , not (null e)
        ]
  -- Create specification information
  ivs <- get
  let specs = groupKeyBy . M.toList . fmap ((:[]) . ixCollectionToSpec ivs) $ arrayAccesses
  tell $ [(span, specs)] -- add to report
  return b
perBlock b@(F.BlDo _ span _ (doSpec@F.DoSpecification {}) body) = do
  let F.DoSpecification _ _ (
          F.StExpressionAssign _ _ (F.ExpValue _ _ (F.ValVariable _ v)) _
        ) _ _ = doSpec
  modify $ union [v] -- introduce v into the list of induction variables
  (cycles, _, _) <- ask

  let lexps = FA.lhsExprs =<< body

  let getTimeSpec e = do
        lhsV <- case e of F.ExpValue _ _ (F.ValVariable _ lhsV)                     -> Just lhsV
                          F.ExpValue _ _ (F.ValArray _ lhsV)                        -> Just lhsV
                          F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValArray _ lhsV)) _ -> Just lhsV
                          _                                                         -> Nothing
        v'   <- lookup lhsV cycles
        return ([lhsV], [TemporalBwd [v']])

  let tempSpecs = foldl' (\ ts -> maybe ts (:ts) . getTimeSpec) [] lexps

  tell $ [(span, tempSpecs)]
  -- descend into the body of the do-statement, with the updated list of induction variables.
  mapM (descendBiM perBlock) body
  -- (we don't need to worry about scope, thanks to renaming)
  return b
perBlock b = return b

-- Penelope's first code, 20/03/2016. 
-- iii././//////////////////////. mvnmmmmmmmmmu

{- *** 2 . Operations on specs, and conversion from indexing expressions -}

-- padZeros makes this rectilinear
padZeros :: [[Int]] -> [[Int]]
padZeros ixss = let m = maximum (map length ixss)
                in map (\ixs -> ixs ++ (replicate (m - (length ixs)) 0)) ixss 
   

-- Convert list of indexing expressions to a spec
ixCollectionToSpec :: [Variable] -> [[F.Expression A]] -> Specification
ixCollectionToSpec ivs ess = snd3 . fromIndicesToSpec . fromLists . padZeros . map toListsOfRelativeIndices $ ess
  where

       
   toListsOfRelativeIndices :: [F.Expression A] -> [Int]
   toListsOfRelativeIndices = fromMaybe [] . mapM (ixExprToOffset ivs)

   -- Convert indexing expressions which are translations to their translation offsett:
   -- e.g., for the expression a(i+1,j-1) then this function gets
   -- passed expr = i + 1   (returning +1) and expr = j - 1 (returning -1)
   ixExprToOffset :: [Variable] -> F.Expression A -> Maybe Int
   ixExprToOffset ivs (F.ExpValue _ _ (F.ValVariable _ v))
     | v `elem` ivs = Just 0
     -- TODO: if we want to capture 'constant' parts, then edit htis
     | otherwise    = Nothing
   ixExprToOffset ivs (F.ExpBinary _ _ F.Addition (F.ExpValue _ _ (F.ValVariable _ v))
                                                       (F.ExpValue _ _ (F.ValInteger offs)))
     | v `elem` ivs = Just $ read offs
   ixExprToOffset ivs (F.ExpBinary _ _ F.Addition (F.ExpValue _ _ (F.ValInteger offs))
                                                    (F.ExpValue _ _ (F.ValVariable _ v)))
     | v `elem` ivs = Just $ read offs
   ixExprToOffset ivs (F.ExpBinary _ _ F.Subtraction (F.ExpValue _ _ (F.ValVariable _ v))
                                                       (F.ExpValue _ _ (F.ValInteger offs)))
     | v `elem` ivs = Just $ if x < 0 then abs x else (- x)
     where x = read offs
   -- TODO: if we want to capture 'constant' parts, then edit htis     
   --ixExprToOffset ivs d (F.ExpValue _ _ (F.ValInteger _)) = Just $ Const d
   ixExprToOffset ivs _ = Nothing

--------------------------------------------------

{- *** 3. Flows-to analysis -}

type Flows = ReaderT FlowsMapTable (State FlowsMap) -- Monad

runFlows :: FlowsMapTable -> Flows a -> (a, FlowsMap)
runFlows fmt = flip runState M.empty . flip runReaderT fmt

-- FlowsMap structure:
-- -- e.g. (v, [a, b]) means that 'a' and 'b' flow to 'v'
type FlowsMap = M.Map Variable [Variable]
type FlowsMapTable = M.Map F.ProgramUnitName (F.ProgramUnit A, FlowsMap)
type Cycles = [(Variable, Variable)]

flowAnalysisArrays :: F.ProgramFile A -> [(F.ProgramUnit A, FlowsMap)]
flowAnalysisArrays pf = M.elems fmt
  where
    F.ProgramFile cm_pus _ = parameterise pf -- identify function/subroutine parameters
    (fmt, _)               = runFlows fmt flowMaker -- intentionally recursive
    flowMaker              = (M.fromList `fmap`) . forM cm_pus $ \ (_, pu) -> do
                               put M.empty
                               res <- flowAnalysisArraysRecur pu
                               return (F.getName pu, res)

flowAnalysisArraysRecur :: F.ProgramUnit A -> Flows (F.ProgramUnit A, FlowsMap)
flowAnalysisArraysRecur p = do
  flowMap  <- get
  p'       <- flowAnalysisArraysStep p
  flowMap' <- get
  if flowMap == flowMap'
    then return (p', flowMap')
    else flowAnalysisArraysRecur p'

flowAnalysisArraysStep :: F.ProgramUnit A -> Flows (F.ProgramUnit A)
flowAnalysisArraysStep pu = transformBiM perBlock pu
  where
    -- FIXME: do function call expression. This is subtle because
    -- function calls can appear as sub-expressions to assignment
    -- statements. Function calls can change the meaning of assignment
    -- statements too. For example, consider this:
    --
    -- a = f(a, b)
    -- ...
    -- function f(a, b)
    --   f = a
    -- end
    --
    -- A naive flows analysis would conclude that ("a", ["a","b"]) but
    -- interprocedural analysis should uncover that ("a", ["a"]) only.
    --
    -- Further question: what should be the correct flows-map output
    -- of "a(i) = a(b(i))", where a and b are arrays?
    --
    -- Don't forget to handle cases like a(i) = f(g(b(i))) either,
    -- where f and g are functions.

    perBlock :: F.Block A -> Flows (F.Block A)
    perBlock = transformBiM perStmt

    lookupList :: Variable -> FlowsMap -> [Variable]
    lookupList v = fromMaybe [] . Map.lookup v

    perStmt :: F.Statement A -> Flows (F.Statement A)
    perStmt f@(F.StExpressionAssign _ _ lhs rhs) = do
      flowMap <- get

      -- Using the parameterisation analysis, rename function &
      -- subroutine parameters to this schema so that they may be
      -- substituted later by actual arguments.
      let p (F.ValArray (A { unitInfo = Just (Parametric (fn, n)) }) _) = fn ++ "[" ++ show n ++ "]"
          p (F.ValArray _ v) = v
      let lhses = [ p v | (F.ExpValue _ _ v@(F.ValArray _ _)) <- universeBi lhs :: [F.Expression A] ]
      let rhses = [ p v | (F.ExpValue _ _ v@(F.ValArray _ _)) <- universeBi rhs :: [F.Expression A] ]

      let pullInFlowsFromRight lhsV map rhsV = M.insertWith union lhsV (lookupList rhsV map) map

      let fromRightToLeft map lhsV = foldl' (pullInFlowsFromRight lhsV)
                                            (M.insertWith union lhsV rhses map)
                                            rhses

      put $ foldl' fromRightToLeft flowMap lhses

      return f

    perStmt f@(F.StCall _ _ (F.ExpValue _ _ (F.ValSubroutineName sn)) (Just argAList)) = do
      let args = F.aStrip argAList
      fmt <- ask

      -- lazily look-up flows analysis of subroutine
      let subFlMap  = maybe M.empty snd $ M.lookup (F.Named sn) fmt

      let doArg n (F.ExpValue _ _ (F.ValVariable _ v)) = [(sn ++ "[" ++ show n ++ "]", v)]
          doArg n (F.ExpValue _ _ (F.ValArray _ v))    = [(sn ++ "[" ++ show n ++ "]", v)]
          doArg _ _                                    = []
      -- assemble necessary substitutions
      let argSubs   = concat $ zipWith doArg [1..] args

      -- apply
      let subFlMap' = transformBi (\ s -> s `fromMaybe` lookup s argSubs) subFlMap

      -- combine with other mappings
      modify $ M.union subFlMap'

      return f
    perStmt f = return f

--------------------------------------------------

-- Find all array accesses which have a cyclic dependency
cyclicDependents :: FlowsMap -> Cycles
cyclicDependents flmap = filter (uncurry (/=)) reflSubset
  where
    self           = flmap `composeRelW` flmap
    -- From M.Map k [(v, k)] return the list of (k, v) which gives
    -- those variables 'k' which have a cylcic depenency through 'v'.
    -- TODO: Possibly only makes sense for paths of length 2: invesitgate length > 2
    reflSubset     = foldl' frob [] (M.assocs self)
    frob p (k, ks) = maybe p ((:p) . (k,)) $ k `lookup` map swap ks

-- Inverts a relation (represented as a map)
--invertRel :: Ord v => Map.Map k [v] -> Map.Map v [k]
--invertRel m = foldl' (\m (k, vs) -> foldl' (\m' v -> Map.insertWith (++) v [k] m') m vs) Map.empty (Map.assocs m)

-- Compose two relations with a witness of where the 'join' point in
-- the middle is.
-- e.g., for two relations R and S, if (a R b) and (b S c) then (a R.S (b, c))
composeRelW :: (Ord k, Ord v) => M.Map k [v] -> M.Map v [k] -> M.Map k [(v, k)]
composeRelW r s = foldl' frob1 M.empty (M.assocs r)
  where
    frob1 rs (k, vs) = foldl' (frob2 k) rs vs
    frob2 k rs' v    = fromMaybe rs' $ do
                         k' <- M.lookup v s
                         return $ M.insertWith (++) k (map (v,) k') rs'

--------------------------------------------------

-- Iterate on action, supplying its return value back to itself, until
-- the action results in Nothing.
iterateMaybe_ :: Monad m => (a -> m (Maybe a)) -> a -> m ()
iterateMaybe_ f x = f x >>= return () `maybe` iterateMaybe_ f

labelEq (Just (F.ExpValue _ _ (F.ValLabel l1))) (Just (F.ExpValue _ _ (F.ValLabel l2))) = l1 == l2
labelEq _ _ = False

lineCol :: FU.Position -> (Int, Int)
lineCol p  = (fromIntegral $ FU.posLine p, fromIntegral $ FU.posColumn p)

spanLineCol :: FU.SrcSpan -> ((Int, Int), (Int, Int))
spanLineCol (FU.SrcSpan l u) = (lineCol l, lineCol u)

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
type TypeEnv a = M.Map FAT.TypeScope (M.Map String FAT.IDType)
isArrayType :: TypeEnv A -> F.ProgramUnitName -> String -> Bool
isArrayType tenv name v = fromMaybe False $ do
  tmap <- M.lookup (FAT.Local name) tenv `mplus` M.lookup FAT.Global tenv
  idty <- M.lookup v tmap
  cty  <- FAT.idCType idty
  return $ cty == FAT.CTArray

coalesceRegions :: [(Int, Int)] -> [(Int, Int)]
coalesceRegions [] = []
coalesceRegions [x] = [x]
coalesceRegions ((x, y) : ((z, a) : zs)) | y == z = (x, a) : (coalesceRegions zs)

--coalesce2D :: [[(Int, Int)]] -> [[(Int, Int)]]
