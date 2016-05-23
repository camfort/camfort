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
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe

{-  Contains the syntax representation for stencil specifications -}

-- The `maxBound` of Int models constants and non-affine / non-induction
-- variable expressions. This can be made smaller for debugging purposes,
-- e.g., 100, but it needs to be high enough to clash with reasonable
-- relative indices.
constantRep = 100 :: Int -- maxBound :: Int

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

emptySpec = Specification (Left emptySpatialSpec)
emptySpatialSpec = Spatial NonLinear [] [] (Sum [Product []])

-- `isEmpty` predicate on which specifications are vacuous or
-- functional empty (i.e., show not be displayed in an inference setting).
isEmpty (Specification (Right (Dependency []))) = True
isEmpty (Specification (Left (Spatial _ irrefl refl (Sum xs)))) =
  irrefl == [] && refl == [] && all emptyOrConstant xs
isEmpty _ = False
emptyOrConstant (Product []) = True
emptyOrConstant (Product xs) =
  all (\xs -> case xs of Constant _ -> True; _ -> False) xs

data Linearity = Linear | NonLinear deriving (Eq, Data, Typeable)

type Dimension  = Int -- spatial dimensions are 1 indexed
type Depth      = Int

-- Individual regions
data Region where
    Forward  :: Depth -> Dimension -> Region
    Backward :: Depth -> Dimension -> Region
    Centered :: Depth -> Dimension -> Region
    Constant :: Dimension -> Region
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

  (Constant dim) <= (Constant dim') = dim <= dim'

  -- Order in the way defined above: Forward <: Backward <: Centered <: Constant
  (Forward _ _ ) <= _               = True
  (Backward _ _) <= (Centered _ _)  = True
  _              <= (Constant _)    = True
  _              <= _               = False

-- Sum of product specifications
newtype RegionSum = Sum [RegionProd]
  deriving (Eq, Data, Typeable)

-- Product of specifications
newtype RegionProd = Product [Region]
  deriving (Eq, Data, Typeable)

instance Ord RegionProd where
   (Product xs) <= (Product xs') = xs <= xs'


regionPlus :: Region -> Region -> Maybe Region
regionPlus (Forward dep dim) (Backward dep' dim')
    | dep == dep' && dim == dim' = Just $ Centered dep dim
regionPlus (Backward dep dim) (Forward dep' dim')
    | dep == dep' && dim == dim' = Just $ Centered dep dim
regionPlus x y | x == y          = Just x
regionPlus x y                   = Nothing

-- If there are two region lists which are equal modulo an entry in
-- one which is `Forward d dim` and `Backward d dim` in the other
equalModuloFwdBwd :: [Region] -> [Region] -> Maybe (Region, [Region])
equalModuloFwdBwd
  ((Forward d dim):rs) ((Backward d' dim'):rs')
    | d == d' && dim == dim' && rs == rs' = Just (Centered d dim, rs)
    | otherwise                           = Nothing
equalModuloFwdBwd
  ((Backward d dim):rs) ((Forward d' dim'):rs')
    = equalModuloFwdBwd ((Forward d' dim'):rs') ((Backward d dim):rs)
equalModuloFwdBwd (r:rs) (r':rs')
    | r == r'   = do (cr, rs'') <- equalModuloFwdBwd rs rs'
                     return (cr, r : rs'')
    | otherwise = Nothing

instance PartialMonoid RegionProd where
   emptyM = Product []

   appendM (Product [])   s  = Just $ s
   appendM s (Product [])    = Just $ s
   appendM (Product [s]) (Product [s']) =
       regionPlus s s' >>= (\sCombined -> return $ Product [sCombined])
   appendM (Product ss) (Product ss')
       | ss == ss' = Just $ Product ss
       | otherwise = case equalModuloFwdBwd ss ss' of
                       Just (s, ss') -> Just $ Product (sort $ s : ss')
                       Nothing       -> Nothing
   appendM _               _ = Nothing


sumLinearity :: Linearity -> Linearity -> Linearity
sumLinearity Linear Linear = Linear
sumLinearity NonLinear _   = NonLinear
sumLinearity _ NonLinear   = NonLinear

sumSpatial :: Spatial -> Spatial -> Spatial
sumSpatial (Spatial lin irdim rdim (Sum ss))
           (Spatial lin' irdim' rdim' (Sum ss')) =
    Spatial (sumLinearity lin lin') (nub $ irdim ++ irdim') (nub $ rdim ++ rdim')
            (Sum $ normalise $ ss ++ ss')

prodSpatial :: Spatial -> Spatial -> Spatial
prodSpatial (Spatial lin irdim rdim s) (Spatial lin' irdim' rdim' s') =
    Spatial (sumLinearity lin lin') (nub $ irdim ++ irdim') (nub $ rdim ++ rdim')
            (prodRegionSum s s')

prodRegionSum :: RegionSum -> RegionSum -> RegionSum
prodRegionSum (Sum ss) (Sum ss') =
   Sum $ -- Take the cross product of list of sumed specifications
     do (Product spec) <- ss
        (Product spec') <- ss'
        return $ Product $ sort $ spec ++ spec'

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
    concat . intersperse ", " . catMaybes $ [refl, irefl, lin, sregion]
    where
      -- Map "empty" spec to Nothing here
      sregion = case show region of
                  "empty" -> Nothing
                  xs      -> Just xs
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
   show (Centered dep dim)  = showRegion "centered" (show dep) (show dim)
   show (Constant dim)      = "constant, dim=" ++ show dim

-- Helper for showing regions
showRegion typ depS dimS = typ ++ ", depth=" ++ depS ++ ", dim=" ++ dimS
