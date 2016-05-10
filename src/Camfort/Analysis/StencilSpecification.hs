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

import Data.Data
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Camfort.Analysis.StencilSpecification.Check
import Camfort.Analysis.StencilSpecification.Inference
import Camfort.Analysis.StencilSpecification.Synthesis
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
import qualified Forpar.Analysis.BBlocks as FAB
import qualified Forpar.Analysis.DataFlow as FAD
import qualified Forpar.Util.Position as FU

import qualified Data.Map as Map
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function (on)
import Data.Maybe
import Data.List
import Data.Tuple (swap)
import Data.Ord

import Debug.Trace

--------------------------------------------------
-- For the purposes of development, a representative example is given by running (in ghci):
--      stencilsInf "samples/stencils/one.f" [] () ()

infer :: F.ProgramFile Annotation -> String
infer = concatMap (formatSpec M.empty) . FAR.underRenaming (infer' . FAB.analyseBBlocks)
infer' pf@(F.ProgramFile cm_pus _) = concatMap perPU cm_pus
  where
    perPU (_, pu) = runInferer cycs2 (F.getName pu) tenv (descendBiM perBlock pu)
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
                     , let Just ms = M.lookup m flMap
                     , n `S.member` ms && n /= m ]
    beMap = FAD.genBackEdgeMap (FAD.dominators gr) gr -- identify every loop by its back-edge
    ivMap = FAD.genInductionVarMap beMap gr -- get [basic] induction variables for every loop
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
                     , m <- S.toList ns
                     , let Just ms = M.lookup m flMap
                     , n `S.member` ms && n /= m ]

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
type Cycles = [(F.Name, F.Name)]
runInferer :: Cycles -> F.ProgramUnitName -> TypeEnv A -> Inferer a -> [LogLine]
runInferer cycles puName tenv =
  flip evalState [] . flip runReaderT (cycles, puName, tenv) . execWriterT

--------------------------------------------------

perBlock :: F.Block (FA.Analysis A) -> Inferer (F.Block (FA.Analysis A))
perBlock b@(F.BlStatement _ span _ (F.StExpressionAssign _ _ _ rhs)) = do
  (_, puName, tenv) <- ask
  -- Get array indexing (on the RHS)
  let rhsExprs = universeBi rhs :: [F.Expression (FA.Analysis A)]
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
ixCollectionToSpec :: [Variable] -> [[F.Expression (FA.Analysis A)]] -> Specification
ixCollectionToSpec ivs ess = snd3 . fromIndicesToSpec . fromLists . padZeros . map toListsOfRelativeIndices $ ess
  where      
   toListsOfRelativeIndices :: [F.Expression (FA.Analysis A)] -> [Int]
   toListsOfRelativeIndices = fromMaybe [] . mapM (ixExprToOffset ivs)

-- Convert indexing expressions which are translations to their translation offsett:
-- e.g., for the expression a(i+1,j-1) then this function gets
-- passed expr = i + 1   (returning +1) and expr = j - 1 (returning -1)
ixExprToOffset :: [Variable] -> F.Expression (FA.Analysis A) -> Maybe Int
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
--ixExprToIndex ivs d (F.ExpValue _ _ (F.ValInteger _)) = Just $ Const d
ixExprToIndex ivs d _ = Nothing

--------------------------------------------------

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

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
