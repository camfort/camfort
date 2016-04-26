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

{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleContexts, ImplicitParams #-}

module Camfort.Analysis.StencilSpecification.LangFort where

import Language.Fortran hiding (Spec)

import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy

import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations (Annotation, unitAnnotation)
import Camfort.Analysis.Syntax (lhsExpr)
import Camfort.Analysis.Types (typeEnv, TypeEnv, isArrayType)

import Camfort.Analysis.StencilSpecification.Inference
import Camfort.Analysis.StencilSpecification.Syntax

import Data.Maybe
import qualified Data.Map as Map
import Data.List
import Camfort.Helpers (spanLineCol)

import Debug.Trace

-- Infer and check stencil specifications
infer :: Program a -> String
infer p = specInference .
          -- First set the unit annotation
          map (fmap (const unitAnnotation)) $ p

check :: Program a -> Program Annotation
check = error "Not yet implemented"

-- For the purposes of development, a representative example is given by running (in ghci):
--      stencilsInf "samples/stencils/one.f90" [] () ()

-- Provides the main inference procedure for specifications which is currently at the level of
-- per statement for spatial stencils, and per for-loop for temporal stencils
specInference :: Program Annotation -> String
specInference p = let flowProgUnitPairs = flowAnalysisArrays p
                  in concatMap specInference' flowProgUnitPairs
-- Helper for appending the specification report information in the state monad
addToReport :: String -> State ([String], [Variable]) ()
addToReport x = modify (\ (y, vs) -> (y ++ [x], vs))

specInference' (p, flMap) =
             let formatSpec span []    = ""
                 formatSpec span specs =
                      show (spanLineCol span) ++ " \t"
                         ++ (concat $ intersperse ", " $ nub $ map (\(arrayVar, spec) -> (concat $ intersperse "," arrayVar) ++ ": " ++ showL spec) specs)
                         ++ "\n"

                 perBlock :: (?cycles :: Cycles) => Block Annotation -> State [String] (Block Annotation)
                 perBlock b =
                   do s <- get
                      let tenv = typeEnv b
                      let (b', (s', _)) = runState (descendBiM (perStmt tenv) b) (s, [])
                      put s'
                      return b'

                 perStmt :: (?cycles :: Cycles) => TypeEnv Annotation -> Fortran Annotation -> State ([String], [Variable]) (Fortran Annotation)
                 -- Match any assignment statements
                 perStmt tenv f@(Assg annotation span lhs@(Var _ _ [(VarName _ lhsV, es)]) rhs) =
                     do -- Get array indexing (on the RHS)
                        let rhsExprs = universeBi rhs :: [Expr Annotation]
                        let arrayAccesses = collect [(v, e) | (Var _ _ [(VarName _ v, e)]) <- rhsExprs,
                                                              length e > 0,
                                                              isArrayType tenv v]
                        -- Create specification information
                        ivs <- gets snd
                        let specs = groupKeyBy $ Map.toList $ fmap ((:[]) . ixCollectionToSpec ivs) $ arrayAccesses
                        addToReport (formatSpec span specs)

                        -- Done
                        return f

                 perStmt tenv f@(For annotation span (VarName _ v) start end inc body) =
                   do modify $ \(r, vs) -> (r, nub (v:vs))
                      ivs <- gets snd
                      -- Insert temporal specs for anything inside the for-loop
                      let tempSpecs = foldl (\ts e ->
                                             case e of
                                                (Var _ _ [(VarName _ lhsV, _)]) ->
                                              -- Insert time specification if there is a cyclic depenency through the assignment (for arrays)
                                                 case (lookup lhsV ?cycles) of
                                                    Just v' -> ([lhsV], [TemporalBwd [v']]) : ts
                                                    Nothing -> ts
                                                _ -> ts) [] (lhsExpr body)
                      addToReport $ formatSpec span tempSpecs

                      descendBiM (perStmt tenv) body -- Descend inside for-loop

                 perStmt tenv f = do mapM (perStmt tenv) (children f) -- Descend
                                     return f

             in let ?cycles = cyclicDependents flMap
                in -- ("---" ++ show ?cycles ++ "\n") `trace`
                   let (_, output) = runState (transformBiM perBlock p) []
                   in concat $ nub output

{- *** 2 . Operations on specs, and conversion from indexing expressions -}

-- Convert list of indexing expressions to list of specs
ixCollectionToSpec :: [Variable] -> [[Expr p]] -> Specification
ixCollectionToSpec ivs ess = snd3 . fromIndicesToSpec . fromLists . padZeros . map toListsOfIndices $ ess
  where

   padZeros :: [[Int]] -> [[Int]]
   padZeros ixss = let m = maximum (map length ixss)
                      in map (\ixs -> ixs ++ (take (m - (length ixs)) [0..])) ixss 
       
   toListsOfIndices :: [Expr p] -> [Int]
   toListsOfIndices = (fromMaybe [] . zipWithM (ixExprToIndex ivs) [0..])

   -- Convert a single index expression for a particular dimension to intermediate spec
   -- e.g., for the expression a(i+1,j+1) then this function gets
   -- passed dim = 0, expr = i + 1 and dim = 1, expr = j + 1
   ixExprToIndex :: [Variable] -> Dimension -> Expr p -> Maybe Int
   ixExprToIndex ivs d (Var _ _ [(VarName _ v, [])])
     | v `elem` ivs = Just $ 0
     -- TODO: if we want to capture 'constant' parts, then edit htis
     | otherwise    = Nothing
   ixExprToIndex ivs d (Bin _ _ (Plus _) (Var _ _ [(VarName _ v, [])]) (Con _ _ offs))
     | v `elem` ivs = Just $ read offs
   ixExprToIndex ivs d (Bin _ _ (Plus _) (Con _ _ offs) (Var _ _ [(VarName _ v, [])]))
     | v `elem` ivs = Just $ read offs
   ixExprToIndex ivs d (Bin _ _ (Minus _) (Var _ _ [(VarName _ v, [])]) (Con _ _ offs))
     | v `elem` ivs = Just $ if x < 0 then abs x else (- x)
     where x = read offs
   -- TODO: if we want to capture 'constant' parts, then edit htis     
   --ixExprToIndex ivs d (F.ExpValue _ _ (F.ValInteger _)) = Just $ Const d
   ixExprToIndex ivs d _ = Nothing

groupKeyBy :: Eq b => [(a, b)] -> [([a], b)]
groupKeyBy xs = groupKeyBy' (map (\(k, v) -> ([k], v)) xs)

groupKeyBy' []                                    = []
groupKeyBy' [(ks, v)]                             = [(ks, v)]
groupKeyBy' ((ks1, v1):((ks2, v2):xs)) | v1 == v2 = groupKeyBy' ((ks1 ++ ks2, v1) : xs)
                                       | otherwise = (ks1, v1) : groupKeyBy' ((ks2, v2) : xs)

{- *** 4. Flows-to analysis -}

-- FlowsMap structure:
-- -- e.g. (v, [a, b]) means that 'a' and 'b' flow to 'v'
type FlowsMap = Map.Map Variable [Variable]
type Cycles = [(Variable, Variable)]

flowAnalysisArrays :: Program Annotation -> [(ProgUnit Annotation, FlowsMap)]
flowAnalysisArrays ps = map (\p -> flowAnalysisArraysRecur p Map.empty) ps

flowAnalysisArraysRecur :: ProgUnit Annotation -> FlowsMap -> (ProgUnit Annotation, FlowsMap)
flowAnalysisArraysRecur p flowMap =
          let (p', flowMap') = runState (flowAnalysisArraysStep p) flowMap
          in --("flowMap' = " ++ show flowMap' ++ "\n") `trace`
              if (flowMap == flowMap') then (p', flowMap')
              else  flowAnalysisArraysRecur p' flowMap'

flowAnalysisArraysStep :: ProgUnit Annotation -> State FlowsMap (ProgUnit Annotation)
flowAnalysisArraysStep p =
         let perBlock :: Block Annotation -> State FlowsMap (Block Annotation)
             perBlock b = let tenv = typeEnv b
                          in transformBiM (perStmt tenv) b

             lookupList :: Variable -> FlowsMap -> [Variable]
             lookupList v map = maybe [] id (Map.lookup v map)

             perStmt :: TypeEnv Annotation -> Fortran Annotation -> State FlowsMap (Fortran Annotation)
             perStmt tenv f@(Assg annotation span lhs rhs) =
                     do let lhses = [v | (Var _ _ [(VarName _ v, es)]) <- (universeBi lhs)::[Expr Annotation], isArrayType tenv v]
                        let rhses = [v | (Var _ _ [(VarName _ v, es)]) <- (universeBi rhs)::[Expr Annotation], isArrayType tenv v]
                        flowMap <- get
                        let pullInFlowsFromRight lhsV map rhsV = Map.insertWith (\a b -> nub (a ++ b)) lhsV (lookupList rhsV map) map
                        let fromRightToLeft           map lhsV = foldl (pullInFlowsFromRight lhsV) (Map.insertWith (++) lhsV rhses map) rhses
                        put $ foldl fromRightToLeft flowMap lhses
                        return f
             perStmt tenv f = return f

        in transformBiM perBlock p

-- Find all array accesses which have a cyclic dependency
cyclicDependents :: FlowsMap -> Cycles
cyclicDependents flmap = let self = flmap `composeRelW` flmap
                             reflSubset = foldl (\p (k, ks) -> case (lookup k (map (\(a, b) -> (b, a)) ks)) of
                                                                  Nothing -> p
                                                                  Just v  -> (k, v) : p) [] (Map.assocs self)
                            -- Remove reflexive dependencies
                         in filter (\(u, v) -> not (u == v)) (reflSubset)

-- Inverts a relation (represented as a map)
--invertRel :: Ord v => Map.Map k [v] -> Map.Map v [k]
--invertRel m = foldl (\m (k, vs) -> foldl (\m' v -> Map.insertWith (++) v [k] m') m vs) Map.empty (Map.assocs m)

-- compose two relations with a witness of where the 'join' point in the middle is
-- e.g., for two relations R and S, if (a R b) and (b S c) then (a R.S (b, c))
composeRelW :: (Ord k, Ord v) => Map.Map k [v] -> Map.Map v [k] -> Map.Map k [(v, k)]
composeRelW r s = foldl (\rs (k, vs) -> foldl (\rs' v -> case (Map.lookup v s) of
                                                           Nothing -> rs'
                                                           Just k' -> Map.insertWith (++) k (map (\k -> (v,k)) k') rs') rs vs) Map.empty (Map.assocs r)
