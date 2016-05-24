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

{-

This files gives an executable implementation of the model for
abstract stencil specifications. This model is used to drive both
the specification checking and program synthesis features.

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}

module Camfort.Analysis.StencilSpecification.Model where

import Camfort.Analysis.StencilSpecification.Syntax
import Data.Set hiding (map,foldl')
import qualified Data.Set as Set
import Data.List
import qualified Data.Map as DM

-- Relative multi-dimensional indices are represented by [Int]
-- e.g. [0, 1, -1] corresponds to a subscript expression a(i, j+1, k-1)
-- Specifications are mapped to (multi)sets of [Int] where
-- the multiset representation is a Map to Bool giving 
-- False = multiplicity 1, True = multiplicity > 1

model :: Specification -> Multiset [Int]
model s = let ?dimensionality = dimensionality s
          in mkModel s

-- Recursive `Model` class implemented for all parts of the spec.
class Model spec where
   type Domain spec

   -- generate model for the specification, where the implicit
   -- parameter ?dimensionality is the global dimensionality
   -- for the spec (not just the local maximum dimensionality)
   mkModel :: (?dimensionality :: Int) => spec -> Domain spec

   -- Return the maximum dimension specified in the spec
   -- giving the dimensionality for that specification
   dimensionality :: spec -> Int

-- | mkSingleEntry offset dimension dimensionality -> relative index vector
-- | precondition: dimensionality >= dimension
mkSingleEntry :: Int -> Int -> Int -> [Int]
mkSingleEntry i 0 ds = error $ "Dimensions are 1-indexed"
mkSingleEntry i 1 ds = [i] ++ take (ds - 1) (repeat 0)
mkSingleEntry i d ds = 0 : mkSingleEntry i (d - 1) (ds - 1)

instance Model Region where
   type Domain Region = Set [Int]

   mkModel (Forward dep dim) =
     fromList [mkSingleEntry i dim ?dimensionality | i <- [0..dep]]

   mkModel (Backward dep dim) =
     fromList [mkSingleEntry i dim ?dimensionality | i <- [(-dep)..0]]

   mkModel (Centered dep dim) =
     fromList [mkSingleEntry i dim ?dimensionality | i <- [(-dep)..dep]]

   mkModel (Constant dim) =
     fromList [mkSingleEntry absoluteRep dim ?dimensionality]

   dimensionality (Forward _ d)  = d
   dimensionality (Backward _ d) = d
   dimensionality (Centered _ d) = d
   dimensionality (Constant d)   = d

instance Model RegionProd where
   type Domain RegionProd = Set [Int]

   mkModel (Product []) = Set.empty
   mkModel (Product ss) =
      fromList $ cprodVs $ map (toList . mkModel) ss

   dimensionality (Product ss) =
      maximum1 (map dimensionality ss)

-- Cartesian product on list of vectors4
cprodVs :: [[[Int]]] -> [[Int]]
cprodVs = foldr1 cprodV

cprodV :: [[Int]] -> [[Int]] -> [[Int]]
cprodV xss yss = xss >>= (\xs -> yss >>= (\ys -> pairwisePerm xs ys))

pairwisePerm :: [Int] -> [Int] -> [[Int]]
pairwisePerm [] [] = []

pairwisePerm [a] [b] | a == absoluteRep = [[absoluteRep]]
                     | b == absoluteRep = [[absoluteRep]]
                     | otherwise        = [[a],[b]]
pairwisePerm (a:as) (b:bs) | a == absoluteRep =
    map (a:) (pairwisePerm as bs)
pairwisePerm (a:as) (b:bs) | b == absoluteRep =
    map (b:) (pairwisePerm as bs)
pairwisePerm (a:as) (b:bs) =
    map (a:) (pairwisePerm as bs)
 ++ map (b:) (pairwisePerm as bs)

instance Model RegionSum where
   type Domain RegionSum = Set [Int]
   mkModel (Sum ss) = unions (map mkModel ss)
   dimensionality (Sum ss) =
     maximum1 (map dimensionality ss)

instance Model Spatial where
    type Domain Spatial = Multiset [Int]

    mkModel (Spatial lin irrefls refls s) =
      case lin of
        Linear    -> DM.fromList . map (,False) . toList $ indices
        NonLinear -> DM.fromList . map (,True) . toList $ indices
       where
        indices = Set.difference
          (Set.union (fromList [mkSingleEntry 0 d ?dimensionality | d <- refls])
                     (mkModel s))
          (fromList [mkSingleEntry 0 d ?dimensionality | d <- irrefls])

    dimensionality (Spatial _ irrefls refls s) =
              maximum1 irrefls
        `max` maximum1 refls
        `max` dimensionality s

-- Multiset representation where multiplicities are (-1) modulo 2
-- that is, False = multiplicity 1, True = multiplicity > 1
type Multiset a = DM.Map a Bool

instance Model Specification where
   type Domain Specification = Multiset [Int]

   mkModel (Specification (Left s)) = mkModel s
   mkModel _                        = error "Only spatial specs are modelled"

   dimensionality (Specification (Left s)) = dimensionality s
   dimensionality _                        = 0

maximum1 [] = 0
maximum1 xs = maximum xs
