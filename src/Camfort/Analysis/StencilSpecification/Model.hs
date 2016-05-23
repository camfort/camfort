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

module Camfort.Analysis.StencilSpecification.Model where

import Camfort.Analysis.StencilSpecification.Syntax
import Data.Set hiding (map,foldl')
import qualified Data.Set as Set
import Data.List
import qualified Data.Map as DM

-- Relative multi-dimensional indices are represented by [Int]
-- e.g. [0, 1, -1] corresponds to a subscript expression a(i, j+1, k-1)
-- Specifications are mapped to (multi)sets of [Int]

class Model spec where
   type Domain spec
   model :: spec -> Domain spec

-- | mkSingleEntry offset dimension -> relative index vector
mkSingleEntry :: Int -> Int -> [Int]
mkSingleEntry i 0 = error $ "Dimensions are 1-indexed"
mkSingleEntry i 1 = [i]
mkSingleEntry i d = 0 : (mkSingleEntry i $ d - 1)

instance Model Region where
   type Domain Region = Set [Int]

   model (Forward dep dim) =
     fromList [mkSingleEntry i dim | i <- [0..dep]]

   model (Backward dep dim) =
     fromList [mkSingleEntry i dim | i <- [(-dep)..0]]

   model (Centered dep dim) =
     fromList [mkSingleEntry i dim | i <- [(-dep)..dep]]

instance Model RegionProd where
   type Domain RegionProd = Set [Int]

   model (Product []) = Set.empty
   model (Product ss) =
      fromList $ cprodVs $ map (toList . model) ss

-- Cartesian product on list of vectors4
cprodVs :: [[[Int]]] -> [[Int]]
cprodVs = foldr1 cprodV

cprodV :: [[Int]] -> [[Int]] -> [[Int]]
cprodV xss yss = prod (normalise xss) (normalise yss)
  where
    findMax = maximum . map length
    dims = max (findMax xss) (findMax yss)
    expand n xs | length xs < n = xs ++ take (n - length xs) (repeat 0)
                | otherwise     = xs
    normalise = map (expand dims)
    prod xss yss =
      xss >>= (\xs -> yss >>= (\ys -> pairwisePerm xs ys))

pairwisePerm :: [a] -> [a] -> [[a]]
pairwisePerm [] [] = []
pairwisePerm [a] [b] = [[a],[b]]
pairwisePerm (a:as) (b:bs) =
    map (a:) (pairwisePerm as bs)
 ++ map (b:) (pairwisePerm as bs)

instance Model RegionSum where
   type Domain RegionSum = Set [Int]
   model (Sum ss) = unions (map model ss)

instance Model Spatial where
   type Domain Spatial = Multiset [Int]

   model (Spatial lin irrefls refls s) =
    case lin of
      Linear    -> DM.fromList . map (,False) . toList $ indices
      NonLinear -> DM.fromList . map (,True) . toList $ indices
    where
      indices =
        Set.difference
          (Set.union (fromList [mkSingleEntry 0 d | d <- refls])
                     (model s))
          (fromList [mkSingleEntry 0 d | d <- irrefls])

-- Multiset representation where multiplicities are (-1) modulo 2
-- that is, False = multiplicity 1, True = multiplicity > 1
type Multiset a = DM.Map a Bool

instance Model Specification where
   type Domain Specification = Multiset [Int]

   model (Specification (Left s)) = model s
   model _                        = error "Only temporal specs are modelled"
