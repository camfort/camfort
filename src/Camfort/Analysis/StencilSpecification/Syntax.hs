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

-- Top-level of specifications: may be either spatial or temporal
data Specification =
  Specification (Either Spatial Temporal)
    deriving (Eq, Data, Typeable)

-- ***********************
-- Temporal specifications:
--   Defines a list of variables which the subject
--   of the specification depends upon
data Temporal = Dependency [String]
    deriving (Eq, Data, Typeable)

-- **********************
-- Spatial specifications:
-- Comprises some modifies on spatial specifications:
--         * linearity
--         * irreflexivity
--         * reflexivity
-- with the region, which is a regionSum
--
-- Regions are in disjunctive normal form (with respect to
--  products on dimensions and sums):
--    i.e., (A * B) U (C * D)...
data Spatial =
   Spatial { modLinearity    :: Linearity,
             modIrreflexives :: [Dimension],
             modReflexives   :: [Dimension],
             region          :: RegionSum }
  deriving (Eq, Data, Typeable)

emptySpec = Spatial NonLinear [] [] (Sum [Product []])

data Linearity = Linear | NonLinear deriving (Eq, Data, Typeable)

type Dimension  = Int -- spatial dimensions are 1 indexed
type Depth      = Int

-- Individual regions
data Region where
    Forward  :: Depth -> Dimension -> Region
    Backward :: Depth -> Dimension -> Region
    Centered :: Depth -> Dimension -> Region
  deriving (Eq, Data, Typeable)

-- An (arbitrary) ordering on regions for the sake of normalisation
instance Ord Region where
  (Forward dep dim) <= (Forward dep' dim')
    | dep == dep' = dim <= dim'
    | otherwise   = dep <= dep'

  (Backward dep dim) <= (Backward dep' dim')
    | dep == dep' = dim <= dim'
    | otherwise   = dep <= dep'

  (Centered dep dim) <= (Centered dep' dim')
    | dep == dep' = dim <= dim'
    | otherwise   = dep <= dep'

  -- Order in the way defined above: Forward <: Backward <: Centered
  (Forward _ _ ) <= _               = True
  (Backward _ _) <= (Centered _ _) = True
  _              <= _               = False

-- Sum of product specifications
newtype RegionSum = Sum [RegionProd]
  deriving (Eq, Data, Typeable)

-- Product of specifications
newtype RegionProd = Product [Region]
  deriving (Eq, Data, Typeable)

instance Ord RegionProd where
   (Product xs) <= (Product xs')
     | length xs == length xs' = xs <= xs'
     | otherwise               = (length xs) <= (length xs')


instance PartialMonoid RegionProd where
   emptyM = Product []

   appendM (Product [])   s  = Just $ s
   appendM s (Product [])    = Just $ s
   appendM (Product ss) (Product ss') = Nothing
--        Just $ Product (ss ++ ss')

sumLinearity :: Linearity -> Linearity -> Linearity
sumLinearity Linear Linear = Linear
sumLinearity NonLinear _   = NonLinear
sumLinearity _ NonLinear   = NonLinear

sumSpatial :: Spatial -> Spatial -> Spatial
sumSpatial (Spatial lin irdim rdim (Sum ss))
           (Spatial lin' irdim' rdim' (Sum ss')) =
    Spatial (sumLinearity lin lin') (irdim ++ irdim') (rdim ++ rdim')
            (Sum $ normalise $ ss ++ ss')

prodSpatial :: Spatial -> Spatial -> Spatial
prodSpatial (Spatial lin irdim rdim s) (Spatial lin' irdim' rdim' s') =
    Spatial (sumLinearity lin lin') (irdim ++ irdim') (rdim ++ rdim')
            (prodRegionSum s s')

prodRegionSum :: RegionSum -> RegionSum -> RegionSum
prodRegionSum (Sum ss) (Sum ss') =
   Sum $ -- Take the cross product of list of sumed specifications
     do (Product spec) <- ss
        (Product spec') <- ss'
        return $ Product $ spec ++ spec'

-- Show a list with ',' separator
showL :: Show a => [a] -> String
showL = concat . (intersperse ",") . (map show)

-- Show lists with '*' or '+' separator (used to represent product of regions)
showProdSpecs, showSumSpecs :: Show a => [a] -> String
showProdSpecs = concat . (intersperse "*") . (map show)
showSumSpecs = concat . (intersperse "+") . (map show)

-- Pretty print top-level specifications
instance Show Specification where
  show (Specification (Left sp)) = "stencil " ++ show sp
  show (Specification (Right sp)) = "stencil " ++ show sp

-- Pretty print spatial specs
instance Show Spatial where
  show (Spatial modLin modIrrefl modRefl region) =
    showMods ++ show region
      where
      -- Show those modifiers which are defined
      showMods = case sequence $ [refl,irefl,lin] of
                   Just mods -> concat . intersperse "," $ mods
                   Nothing   -> ""

      -- Individual actions to show modifiers

      refl = case modRefl of
                []       -> Nothing
                ds       -> Just $ "reflexive, dims=" ++ showL ds
      irefl = case modIrrefl of
                []       -> Nothing
                ds       -> Just $ "irreflexive, dims=" ++ showL ds
      lin = case modLin of
                NonLinear -> Nothing
                Linear    -> Just $ "readOnce"

-- Pretty print temporal specs
instance Show Temporal where
    show (Dependency vars) = "dependency " ++ showL vars

-- Pretty print region sums
instance Show RegionSum where
    -- Tweedle-dum
    show (Sum []) = "empty"
    -- Tweedle-dee
    show (Sum [Product []]) = "empty"

    show (Sum specs) =
      concat $ intersperse " + " ppspecs
      where ppspecs = filter ((/=) "") $ map show specs

instance Show RegionProd where
    show (Product []) = ""
    show (Product ss)  =
       concat . (intersperse "*") . (map ((\s -> "(" ++ show s ++ ")"))) $ ss

instance Show Region where
   show (Forward dep dim)   = showRegion "forward" (show dep) (show dim)
   show (Backward dep dim)  = showRegion "backward" (show dep) (show dim)
   show (Centered dep dim) = showRegion "centered" (show dep) (show dim)

-- Helper for showing regions
showRegion typ depS dimS = typ ++ ", depth=" ++ depS ++ ", dim=" ++ dimS
