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
import Data.List hiding (sum)
import Prelude hiding (sum)
import Data.Maybe

{-  Contains the syntax representation for stencil specifications -}

{- *** 0. Representations -}

-- Representation of an inference result, either exact or with some bound
data Result a =
  Exact a | Bound (Maybe a) (Maybe a)
   deriving (Eq, Data, Typeable, Show)

fromExact :: Result a -> a
fromExact (Exact a) = a
fromExact _ = error $ "Exception: fromExact on a non-exact result"

upperBound :: a -> Result a
upperBound x = Bound Nothing (Just x)

lowerBound :: a -> Result a
lowerBound x = Bound (Just x) Nothing

instance Functor Result where
  fmap f (Exact x) = Exact (f x)
  fmap f (Bound x y) = Bound (fmap f x) (fmap f y)

-- 'absoluteRep' is an integer to use to represent absolute indexing expressions
-- (which may be constants, non-affine indexing expressions, or expressions
--  involving non-induction variables). This is set to maxBoound :: Int usually,
-- but can be made smaller for debugging purposes,
-- e.g., 100, but it needs to be high enough to clash with reasonable
-- relative indices.
absoluteRep = 100 :: Int -- maxBound :: Int

{- *** 1 . Specification syntax -}

-- Top-level of specifications: may be either spatial or temporal
data Specification =
  Specification (Either (Result Spatial) Temporal)
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

emptySpec = Specification . Left $ (one :: Result Spatial)
emptySpatialSpec = one :: Spatial

-- `isEmpty` predicate on which specifications are vacuous or
-- functional empty (i.e., show not be displayed in an inference setting).
isEmpty :: Specification -> Bool
isEmpty (Specification (Right (Dependency []))) = True
isEmpty (Specification (Left s)) = isUnit s

data Linearity = Linear | NonLinear deriving (Eq, Data, Typeable)

type Dimension  = Int -- spatial dimensions are 1 indexed
type Depth      = Int

-- Individual regions
data Region where
    Forward  :: Depth -> Dimension -> Region
    Backward :: Depth -> Dimension -> Region
    Centered :: Depth -> Dimension -> Region
  deriving (Eq, Data, Typeable)

getDimension :: Region -> Dimension
getDimension (Forward _ dim) = dim
getDimension (Backward _ dim) = dim
getDimension (Centered _ dim) = dim

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

  -- Order in the way defined above: Forward <: Backward <: Centered <: Constant
  (Forward _ _ ) <= _               = True
  (Backward _ _) <= (Centered _ _)  = True
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


-- Operations on region specifications form a semiring
--  where `sum` is the additive, and `prod` is the multiplicative
--  [without the annihilation property for `zero` with multiplication]
class RegionRig t where
  sum  :: t -> t -> t
  prod :: t -> t -> t
  one  :: t
  zero :: t
  isUnit :: t -> Bool

-- Lifting to the `Maybe` constructor
instance RegionRig a => RegionRig (Maybe a) where
  sum (Just x) (Just y) = Just $ sum x y
  sum x Nothing = x
  sum Nothing x = x

  prod (Just x) (Just y) = Just $ prod x y
  prod x Nothing = x
  prod Nothing x = x

  one  = Just one
  zero = Just zero

  isUnit Nothing = True
  isUnit (Just x) = isUnit x

instance RegionRig Linearity where
  sum Linear Linear = Linear
  sum _  _          = NonLinear
  prod = sum
  one  = Linear
  zero = Linear

  isUnit Linear = True
  isUnit _      = False

instance RegionRig Spatial where
  sum (Spatial lin  irdim  rdim  s)
      (Spatial lin' irdim' rdim' s') =
    Spatial (sum lin lin') (nub $ irdim ++ irdim') (nub $ rdim ++ rdim')
            (sum s s')

  prod (Spatial lin  irdim  rdim  s)
       (Spatial lin' irdim' rdim' s') =
    Spatial (prod lin lin') (nub $ irdim ++ irdim') (nub $ rdim ++ rdim')
            (prod s s')

  one = Spatial one [] [] one
  zero = Spatial zero [] [] zero

  isUnit (Spatial _ irrefl refl ss) =
      irrefl == [] && refl == [] && (isUnit ss)

instance RegionRig (Result Spatial) where
  sum (Exact s) (Exact s')      = Exact (sum s s')
  sum (Exact s) (Bound l u)     = Bound (sum (Just s) l) (sum (Just s) u)
  sum (Bound l u) (Bound l' u') = Bound (sum l l') (sum (sum l u') (sum l' u))
  sum s s'                      = sum s' s

  prod (Exact s) (Exact s')     = Exact (prod s s')
  prod (Exact s) (Bound l u)    = Bound (prod (Just s) l) (prod (Just s) u)
  prod (Bound l u) (Bound l' u') = Bound (prod l l') (prod (prod l u') (prod l' u))
  prod s s'                      = prod s' s

  one  = Exact one
  zero = Exact zero

  isUnit (Exact s) = isUnit s
  isUnit (Bound x y) = isUnit x && isUnit y

instance RegionRig RegionSum where
  prod (Sum ss) (Sum ss') =
   Sum $ -- Take the cross product of list of summed specifications
     do (Product spec) <- ss
        (Product spec') <- ss'
        return $ Product $ sort $ spec ++ spec'
  sum (Sum ss) (Sum ss') = Sum $ normalise $ ss ++ ss'
  zero = Sum []
  one = Sum [Product []]
  isUnit s@(Sum ss) = s == zero || s == one || (all ((==) (Product [])) ss)

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

instance {-# OVERLAPS #-} Show (Result Spatial) where
  show (Exact s) = show s
  show (Bound Nothing Nothing) = "empty"
  show (Bound Nothing (Just s)) = "atMost, " ++ show s
  show (Bound (Just s) Nothing) = "atLeast, " ++ show s
  -- TODO: Figure out the right way to support this
  show (Bound (Just sL) (Just sU)) = error $ "Lower and upper, l = " ++ show sL ++ ", u = " ++ show sU

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

-- Helper for showing regions
showRegion typ depS dimS = typ ++ ", depth=" ++ depS ++ ", dim=" ++ dimS
