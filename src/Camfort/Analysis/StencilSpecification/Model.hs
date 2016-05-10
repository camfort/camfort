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

{-# LANGUAGE TypeFamilies, TupleSections #-}

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

-- | mkVec offset dimension -> relative index vector
mkVec :: Int -> Int -> [Int]
mkVec i 0 = error $ "Dimensions are 1-indexed"
mkVec i 1 = [i]
mkVec i d = 0 : (mkVec i $ d - 1)

instance Model Spec where
   type Domain Spec = Set [Int]
 
   model (Forward dep dims) = 
     fromList . cprodVs $ [[mkVec i d | i <- [0..dep]] | d <- dims]
     
   model (Backward dep dims) =
     fromList . cprodVs $ [[mkVec i d | i <- [(-dep)..0]] | d <- dims]
     
   model (Symmetric dep dims) =
     fromList . cprodVs $ [[mkVec i d | i <- [(-dep)..dep]] | d <- dims]
     
   model (Constant dims) = error "No model yet"

instance Model SpecProd where
   type Domain SpecProd = Set [Int]
   
   model (Product []) = Set.empty
   model (Product ss) =
      fromList $ cprodVs $ map (toList . model) ss

-- Cartesian product on list of vectors
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

    pairwisePerm [] [] = []
    pairwisePerm [a] [b] = [[a],[b]]
    pairwisePerm (a:as) (b:bs) =
         map (a:) (pairwisePerm as bs)
      ++ map (b:) (pairwisePerm as bs)

instance Model SpecSum where
   type Domain SpecSum = Set [Int]
   model (Sum ss) = unions (map model ss)

instance Model SpatialSpec where
   type Domain SpatialSpec = Set [Int]
   
   model (SpatialSpec irrefls refls s) =
      Set.difference
        (Set.union (fromList [mkVec 0 d | d <- refls]) (model s))
        (fromList [mkVec 0 d | d <- irrefls])

-- Multiset representation where multiplicities are (-1) modulo 2
-- that is, False = multiplicity 1, True = multiplicity > 1
type Multiset a = DM.Map a Bool 

instance Model Specification where
   type Domain Specification = Multiset [Int]
   
   model (Linear s)    = DM.fromList . map (,False) . toList . model $ s
   model (NonLinear s) = DM.fromList . map (,True) . toList . model $ s         
   model Empty         = DM.empty
   model _             = error "Only temporal specs are modelled"



-- Local test

variations =
        [ ([ (0,0) ], NonLinear $ SpatialSpec [] [ 1, 2 ] (Sum [Product []]))
        , ([ (1,0), (0,0) ], NonLinear $ SpatialSpec [] [2] (Sum [Product [Forward 1 [ 1 ]]]))
        , ([ (0,1), (0,0) ], NonLinear $ SpatialSpec [] [1] (Sum [Product [Forward 1 [ 2 ]]]))
        , ([ (1,1), (0,1), (1,0), (0,0) ], NonLinear $ SpatialSpec [] [] (Sum [Product [Forward 1 [ 1, 2 ]]]))
        , ([ (-1,0), (0,0) ], NonLinear $ SpatialSpec [] [2] (Sum [Product [Backward 1 [ 1 ]]]))
        , ([ (0,-1), (0,0) ], NonLinear $ SpatialSpec [] [1] (Sum [Product [Backward 1 [ 2 ]]]))
        , ([ (-1,-1), (0,-1), (-1,0), (0,0) ], NonLinear $ SpatialSpec [] [] (Sum [Product [Backward 1 [ 1, 2 ]]]))
        , ( [ (0,-1), (1,-1), (0,0), (1,0), (1,1), (0,1), (2,-1), (2,0), (2,1) ]
          , NonLinear $ SpatialSpec [] [] (Sum [Product [ Forward 2 [ 1 ], Symmetric 1 [ 2 ] ] ] ))
        , ( [ (-1,0), (-1,1), (0,0), (0,1), (1,1), (1,0), (-1,2), (0,2), (1,2) ]
          , NonLinear $ SpatialSpec [] [] (Sum [Product [ Forward 2 [ 2 ], Symmetric 1 [ 1 ] ] ] ))          
         -- Stencil which is non-contiguous from the origin in both directions
        , ([ (0, 1), (1, 1) ], NonLinear $ SpatialSpec [] [] (Sum [Product [Forward 1 [ 1 ]]]))
        ]

check = mapM_ check' variations
  where check' (ixs, spec) = putStrLn $ show (sort ixs == sort mdl, sort ixs, sort mdl)
          where mdl = nub $ map toPair $ DM.toList $ model spec
                toPair ([x, y],_) = (x, y)
                toPair ([x],_)    = (x, 0)
                toPair (xs,_)     = error $ "Got " ++ show xs
