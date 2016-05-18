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

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Camfort.Analysis.StencilSpecification.Syntax where

import Camfort.Helpers
import Data.Generics.Uniplate.Data
import Data.List
import Data.Data

{-  Contains the syntax representation for stencil specifications -}


{- *** 1 . Specification syntax -}

-- Top-level of specifications: wraps SpatialSpec with linearity information
data Specification =
    Linear    SpatialSpec
  | NonLinear SpatialSpec
  | TemporalFwd [String]
  | TemporalBwd [String]
  | Empty
  deriving (Eq, Data, Typeable)

-- Wrap a spatial specication with its reflexivity/irreflexivity information
-- Spatial specifications are in disjunctive normal form (with respect to
--  products on dimensions and sums):
--    i.e., (A * B) U (C * D)...
data SpatialSpec =
    SpatialSpec { irreflexives :: [Dimension],
                  reflexives   :: [Dimension],
                  spatial      :: SpecSum }
  deriving (Eq, Data, Typeable)

emptySpec = SpatialSpec [] [] (Summation [Product []])

type Dimension  = Int -- spatial dimensions are 1 indexed
type Depth      = Int

data Op = Prod | Sum

-- The inner elements of a specification
data Spec (op :: Op) where
    Forward   :: Depth -> [Dimension] -> Spec op
    Backward  :: Depth -> [Dimension] -> Spec op
    Symmetric :: Depth -> [Dimension] -> Spec op
  deriving (Eq, Data, Typeable)

deriving instance (Typeable (Spec Sum))
deriving instance (Typeable (Spec Prod))

-- Product of specifications
newtype SpecProd = Product [Spec Prod]
  deriving (Eq, Data, Typeable)

-- Sum of product specifications
newtype SpecSum = Summation [SpecProd]
  deriving (Eq, Data, Typeable)

injectSpec ss = SpatialSpec [] [] (Summation ss)

-- An (arbitrary) ordering on specifications for the sake of normalisation
instance Ord (Spec op) where
  (Forward dep dims) <= (Forward dep' dims')
    | dep == dep' = (sort dims) <= (sort dims')
    | otherwise   = dep <= dep'

  (Backward dep dims) <= (Backward dep' dims')
    | dep == dep' = (sort dims) <= (sort dims')
    | otherwise   = dep <= dep'

  (Symmetric dep dims) <= (Symmetric dep' dims')
    | dep == dep' = (sort dims) <= (sort dims')
    | otherwise   = dep <= dep'

  -- Order in the way defined above: Forward <: Backward <: Symmetric
  (Forward _ _ ) <= _               = True
  (Backward _ _) <= (Symmetric _ _) = True
  _              <= _               = False

instance Ord SpecProd where
   (Product xs) <= (Product xs')
     | length xs == length xs' = xs <= xs'
     | otherwise               = (length xs) <= (length xs')

-- `appendM` combines specs by coalescing specifications with the same
-- direction and depth into one
--   e.g. forward, depth=1, dims=1 `appendM` forward, depth=1,dims=2
--       = forward, depth=1, dims=1 op 2

instance PartialMonoid (Spec op) where

  emptyM = error "No unit"

  appendM (Forward dep dims) (Forward dep' dims')
    | dep == dep' = Just $ Forward dep (sort $ dims ++ dims')
  appendM (Backward dep dims) (Backward dep' dims')
    | dep == dep' = Just $ Backward dep (sort $ dims ++ dims')
  appendM (Symmetric dep dims) (Symmetric dep' dims')
    | dep == dep' = Just $ Symmetric dep (sort $ dims ++ dims')
  appendM x y             = Nothing


{-
sumSpec :: Specification -> Specification -> Specification
sumSpec Empty x = x
sumSpec x Empty = x
sumSpec (Linear ss) (Linear ss')       = inferLinearity $ sumSpatialSpec ss ss'
sumSpec (Linear ss) (NonLinear ss')    = NonLinear $ sumSpatialSpec ss ss'
sumSpec (NonLinear ss) (Linear ss')    = NonLinear $ sumSpatialSpec ss ss'
sumSpec (NonLinear ss) (NonLinear ss') = NonLinear $ sumSpatialSpec ss ss'
-}

instance PartialMonoid SpecProd where
   emptyM = Product []

   appendM (Product [])   s  = Just $ s
   appendM s (Product [])    = Just $ s
   appendM (Product [ss]) (Product [ss']) =
      appendM ss ss' >>= (\ss'' -> return $ Product [ss''])
   appendM _              _  = Nothing


sumSpatialSpec :: SpatialSpec -> SpatialSpec -> SpatialSpec
sumSpatialSpec (SpatialSpec irdim rdim (Summation ss)) (SpatialSpec irdim' rdim' (Summation ss')) =
    SpatialSpec (irdim ++ irdim') (rdim ++ rdim') (Summation $ normalise $ ss ++ ss')

prodSpatialSpec :: SpatialSpec -> SpatialSpec -> SpatialSpec
prodSpatialSpec (SpatialSpec irdim rdim s) (SpatialSpec irdim' rdim' s') =
    SpatialSpec (irdim ++ irdim') (rdim ++ rdim') (prodSpecSum s s')

prodSpecSum :: SpecSum -> SpecSum -> SpecSum
prodSpecSum (Summation ss) (Summation ss') =
   Summation $ -- Take the cross product of list of sumed specifications
     do (Product spec) <- ss
        (Product spec') <- ss'
        return $ Product $ normalise $ spec ++ spec'

-- Show a list with ',' separator
showL :: Show a => [a] -> String
showL = concat . (intersperse ",") . (map show)

-- Show lists with '*' or '+' separator (used to represent product of regions)
showProdSpecs, showSumSpecs :: Show a => [a] -> String
showProdSpecs = concat . (intersperse "*") . (map show)
showSumSpecs = concat . (intersperse "+") . (map show)

-- Pretty-printed syntax

instance Show Specification where
    show (Linear spec)        = (show spec) ++ ", read-once"
    show (NonLinear spec)     = show spec
    show Empty                = "none"
    show (TemporalFwd tdims)  = showRegion "forward" (show $ length tdims) ("t{" ++ showL tdims ++ "}")
    show (TemporalBwd tdims)  = showRegion "backward" (show $ length tdims) ("t{" ++ showL tdims ++ "}")

instance Show SpatialSpec where
    -- Tweedle-dum
    show (SpatialSpec [] [] (Summation [])) = "none"
    -- Tweedle-dee
    show (SpatialSpec [] [] (Summation [Product []])) = "none"

    show (SpatialSpec irdims rdims (Summation specs)) =
      concat $ intersperse " + " ppspecs
      where ppspecs = irspec ++ rspec ++ ppspecs'
            irspec  = if irdims /= [] then ["irreflexive, dims=" ++ showL irdims] else []
            rspec   = if rdims /= [] then ["reflexive, dims=" ++ showL rdims] else []
            ppspecs' = filter ((/=) "") $ map show specs


{-  For products of regions, we provide a compact pretty-printed output that coalesces specifications
     of the same depth and direction

   e.g. (forward, depth=1, dim=1) * (forward, depth=1, dim=2)
   is actually output as
        forward, depth=1, dim=1*2

  This is done via the specPlus normalisation and a custom pretty-printing routine triggered
  by the InsideProd wrapepr which interprets normalised specs as produts of specs -}
instance Show SpecProd where
    show (Product []) = ""
    show (Product ss)  =
       concat . (intersperse "*") . (map ((\s -> "(" ++ show s ++ ")"))) $ ss

instance Show (Spec Prod) where
   show (Forward dep dims)   = showRegion "forward" (show dep) (showProdSpecs dims)
   show (Backward dep dims)  = showRegion "backward" (show dep) (showProdSpecs dims)
   show (Symmetric dep dims) = showRegion "symmetry" (show dep) (showProdSpecs dims)

-- Helper for showing regions
showRegion typ depS dimS = typ ++ ", depth=" ++ depS ++ ", dim=" ++ dimS

instance Show (Spec Sum) where
    show (Forward dep dims)   = showRegion "forward" (show dep) (showSumSpecs dims)
    show (Backward dep dims)  = showRegion "backward" (show dep) (showSumSpecs dims)
    show (Symmetric dep dims) = showRegion "centered" (show dep) (showSumSpecs dims)
