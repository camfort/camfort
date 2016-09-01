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

module Camfort.Specification.Stencils.Model where

import Camfort.Specification.Stencils.Syntax
import Data.Set hiding (map,foldl',(\\))
import qualified Data.Set as Set
import Data.List
import qualified Data.List as DL
import qualified Data.Map as DM

{-| This function maps inner representation to a set of vectors of length
-   given by `dim`. This is the mathematical representation of the
-   specification. |-}
model :: Multiplicity (Approximation Spatial)
      -> Int
      -> Multiplicity (Approximation (Set [Int]))
model s dims =
    let ?globalDimensionality = dims
    in mkModel s

consistent :: Multiplicity [[Int]]
           -> Multiplicity (Approximation Spatial)
           -> Bool
-- If the specification says "readOnce" but there are duplicates among
-- offsets, then there is no consistency.
--
-- Note that if the spec omits "readOnce" and the offsets happen to be
-- unique that is allowed as "readOnce" is an extra qualifier.
consistent (Multiple _) (Single _) = False
consistent mult1 spec =
    consistent' (model spec dimensionality)
  where
    dimensionality = length . head $ accesses
    consistent' m2 =
      case fromMult m2 of
        Exact unifiers ->
          consistent' (Multiple (Bound Nothing (Just unifiers))) &&
          consistent' (Multiple (Bound (Just unifiers) Nothing))
        Bound lus@Just{} uus@Just{} ->
          consistent' (Multiple (Bound lus Nothing)) &&
          consistent' (Multiple (Bound Nothing uus))
        Bound Nothing (Just unifiers) ->
          all (\access -> any (access `accepts`) unifiers) accesses
        Bound (Just unifiers) Nothing ->
          all (\unifier -> any (`accepts` unifier) accesses) unifiers

    accesses = fromMult mult1

    access `accepts` unifier =
      all (\(u,v) -> v == absoluteRep || u == v) (zip access unifier)

-- Recursive `Model` class implemented for all parts of the spec.
class Model spec where
   type Domain spec

   -- generate model for the specification, where the implicit
   -- parameter ?globalDimensionality is the global dimensionality
   -- for the spec (not just the local maximum dimensionality)
   mkModel :: (?globalDimensionality :: Int) => spec -> Domain spec

   -- Return the maximum dimension specified in the spec
   -- giving the dimensionality for that specification
   dimensionality :: spec -> Int
   dimensionality = maximum . dimensions
   -- Return all the dimensions specified for in this spec
   dimensions :: spec -> [Int]

-- Set representation where multiplicities are (-1) modulo 2
-- that is, False = multiplicity 1, True = multiplicity > 1
instance Model Specification where
   type Domain Specification = Multiplicity (Approximation (Set [Int]))

   mkModel (Specification s) = mkModel s

   dimensionality (Specification s) = dimensionality s

   dimensions (Specification s) = dimensions s

instance Model (Multiplicity (Approximation Spatial)) where
   type Domain (Multiplicity (Approximation Spatial)) =
     Multiplicity (Approximation (Set [Int]))

   mkModel (Multiple s) = Multiple (mkModel s)
   mkModel (Single s) = Single (mkModel s)

   dimensionality mult = dimensionality $ fromMult mult

   dimensions mult = dimensions $ fromMult mult

instance Model (Approximation Spatial) where
  type Domain (Approximation Spatial) = Approximation (Set [Int])

  mkModel = fmap mkModel
  dimensionality (Exact s) = dimensionality s
  dimensionality (Bound l u) = dimensionality l `max` dimensionality u

  dimensions (Exact s) = dimensions s
  dimensions (Bound l u) = dimensions l ++ dimensions u

-- Lifting of model to Maybe type
instance Model a => Model (Maybe a) where
  type Domain (Maybe a) = Maybe (Domain a)

  mkModel Nothing = Nothing
  mkModel (Just x) = Just (mkModel x)

  dimensions Nothing = [0]
  dimensions (Just x) = dimensions x

-- Core part of the model
instance Model Spatial where
    type Domain Spatial = Set [Int]

    mkModel (Spatial s) = mkModel s

    dimensionality (Spatial s) = dimensionality s

    dimensions (Spatial s)     = dimensions s


instance Model RegionSum where
   type Domain RegionSum = Set [Int]
   mkModel (Sum ss) = unions (map mkModel ss)
   dimensionality (Sum ss) =
     maximum1 (map dimensionality ss)

   dimensions (Sum ss) = concatMap dimensions ss


instance Model Region where
   type Domain Region = Set [Int]

   mkModel (Forward dep dim reflx) = fromList
     [mkSingleEntryNeg i dim ?globalDimensionality | i <- [i0..dep]]
       where i0 = if reflx then 0 else 1

   mkModel (Backward dep dim reflx) = fromList
     [mkSingleEntryNeg i dim ?globalDimensionality | i <- [(-dep)..i0]]
       where i0 = if reflx then 0 else -1

   mkModel (Centered dep dim reflx) = fromList
     [mkSingleEntryNeg i dim ?globalDimensionality | i <- [(-dep)..dep] \\ i0]
       where i0 = if reflx then [] else [0]

   dimensionality (Forward  _ d _) = d
   dimensionality (Backward _ d _) = d
   dimensionality (Centered _ d _) = d

   dimensions (Forward _ d _)  = [d]
   dimensions (Backward _ d _) = [d]
   dimensions (Centered _ d _) = [d]

mkSingleEntryNeg :: Int -> Int -> Int -> [Int]
mkSingleEntryNeg i 0 ds = error "Dimensions are 1-indexed"
mkSingleEntryNeg i 1 ds = i : replicate (ds - 1) absoluteRep
mkSingleEntryNeg i d ds = absoluteRep : mkSingleEntryNeg i (d - 1) (ds - 1)

instance Model RegionProd where
   type Domain RegionProd = Set [Int]

   mkModel (Product [])  = Set.empty
   mkModel (Product [s])  = mkModel s
   mkModel p@(Product ss)  = cleanedProduct
     where
       cleanedProduct = fromList $ DL.filter keepPred product
       product = cprodVs $ map (toList . mkModel) ss
       dims = dimensions p
       keepPred el = DL.foldr (\pr acc -> nonProdP pr && acc) True (zip [(1::Int)..] el)
       nonProdP (i,el) = i `notElem` dims || el /= absoluteRep

   dimensionality (Product ss) =
      maximum1 (map dimensionality ss)
   dimensions (Product ss) =
      nub $ concatMap dimensions ss

tensor n s t = cleanedProduct
   where
       cleanedProduct = fromList $ DL.filter keepPred product
       product = cprodV s t
       keepPred el = DL.foldr (\pr acc -> nonProdP pr && acc) True (zip [(1::Int)..] el)
       nonProdP (i,el) = i `notElem` [1..n] || el /= absoluteRep

-- Cartesian product on list of vectors4
cprodVs :: [[[Int]]] -> [[Int]]
cprodVs = foldr1 cprodV

cprodV :: [[Int]] -> [[Int]] -> [[Int]]
cprodV xss yss = xss >>= (\xs -> yss >>= pairwisePerm xs)

pairwisePerm :: [Int] -> [Int] -> [[Int]]
pairwisePerm x y = sequence . transpose $ [x, y]

maximum1 [] = 0
maximum1 xs = maximum xs
