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

module Camfort.Specification.Stencils.Syntax
  (
    -- * Datatypes and Aliases
    Linearity(..)
  , Region(..)
  , RegionDecl
  , RegionEnv
  , RegionProd(..)
  , RegionSum(..)
  , Spatial(..)
  , SpecDecl
  , SpecDecls
  , Specification(..)
  , IsStencil
  , Variable
    -- * Functions
  , absoluteRep
  , fromBool
  , groupKeyBy
  , hasDuplicates
  , isEmpty
  , isUnit
  , pprintSpecDecls
  , setLinearity
  ) where

import Camfort.Specification.Stencils.Model ( Multiplicity(..)
                                            , peel
                                            , Approximation(..)
                                            )

import Prelude hiding (sum)

import Data.Data
import Data.List hiding (sum)
import Control.Applicative

type Variable = String

{-  Contains the syntax representation for stencil specifications -}

{- *** 0. Representations -}

-- 'absoluteRep' is an integer to use to represent absolute indexing expressions
-- (which may be constants, non-affine indexing expressions, or expressions
--  involving non-induction variables). This is set to maxBoound :: Int usually,
-- but can be made smaller for debugging purposes,
-- e.g., 100, but it needs to be high enough to clash with reasonable
-- relative indices.
absoluteRep = maxBound :: Int

{- *** 1 . Specification syntax -}

type RegionDecl = (Variable, RegionSum)
type SpecDecl   = ([Variable], Specification)

-- List of region sums associated to region variables
type RegionEnv = [(Variable, RegionSum)]

-- List of specifications associated to variables
-- This is not a map so there might be multiple entries for each variable
-- use `lookupAggregate` to access it
type SpecDecls = [SpecDecl]

pprintSpecDecls :: SpecDecls -> String
pprintSpecDecls =
 concatMap (\(names, spec) ->
            show spec ++ " :: " ++ intercalate "," names ++ "\n")

-- Top-level of specifications: may be either spatial or temporal

-- | `isStencil` is used to mark whether a specification is associated
-- | with a stencil computation, or a general array computation
type IsStencil = Bool

data Specification =
  Specification (Multiplicity (Approximation Spatial)) IsStencil
    deriving (Eq, Data, Typeable)

isEmpty :: Specification -> Bool
isEmpty (Specification mult _) = isUnit . peel $ mult

-- **********************
-- Spatial specifications:
-- is a regionSum
--
-- Regions are in disjunctive normal form (with respect to
--  products on dimensions and sums):
--    i.e., (A * B) U (C * D)...
data Spatial = Spatial RegionSum
  deriving (Eq, Data, Typeable)

-- Helpers for dealing with linearity information

-- A boolean is used to represent multiplicity in the backend
-- with False = multiplicity=1 and True = multiplicity > 1
fromBool :: Bool -> Linearity
fromBool True = NonLinear
fromBool False = Linear

hasDuplicates :: Eq a => [a] -> ([a], Bool)
hasDuplicates xs = (nub xs, nub xs /= xs)

setLinearity :: Linearity -> Specification -> Specification
setLinearity l (Specification mult isStencil)
  | l == Linear    = Specification (Once $ peel mult) isStencil
  | l == NonLinear = Specification (Mult $ peel mult) isStencil

data Linearity = Linear | NonLinear deriving (Eq, Data, Typeable)

type Dimension  = Int -- spatial dimensions are 1 indexed
type Depth      = Int
type IsRefl     = Bool

-- Individual regions
data Region where
    Forward  :: Depth -> Dimension -> IsRefl -> Region
    Backward :: Depth -> Dimension -> IsRefl -> Region
    Centered :: Depth -> Dimension -> IsRefl -> Region
  deriving (Eq, Data, Typeable)

-- An (arbitrary) ordering on regions for the sake of normalisation
instance Ord Region where
  (Forward dep dim _) <= (Forward dep' dim' _)
    | dep == dep' = dim <= dim'
    | otherwise   = dep <= dep'

  (Backward dep dim _) <= (Backward dep' dim' _)
    | dep == dep' = dim <= dim'
    | otherwise   = dep <= dep'

  (Centered dep dim _) <= (Centered dep' dim' _)
    | dep == dep' = dim <= dim'
    | otherwise   = dep <= dep'

  -- Order in the way defined above: Forward <: Backward <: Centered
  Forward{}  <= _          = True
  Backward{} <= Centered{} = True
  _          <= _          = False

-- Product of specifications
newtype RegionProd = Product {unProd :: [Region]}
  deriving (Eq, Data, Typeable)

-- Sum of product specifications
newtype RegionSum = Sum {unSum :: [RegionProd]}
  deriving (Eq, Data, Typeable)

instance Ord RegionProd where
   (Product xs) <= (Product xs') = xs <= xs'


-- Operations on specifications

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

instance RegionRig Spatial where
  sum (Spatial s) (Spatial s') = Spatial (sum s s')

  prod (Spatial s) (Spatial s') = Spatial (prod s s')

  one = Spatial one
  zero = Spatial zero

  isUnit (Spatial ss) = isUnit ss

instance RegionRig (Approximation Spatial) where
  sum (Exact s) (Exact s')      = Exact (sum s s')
  sum (Exact s) (Bound l u)     = Bound (sum (Just s) l) (sum (Just s) u)
  sum (Bound l u) (Bound l' u') = Bound (sum l l') (sum u u')
  sum s s'                      = sum s' s

  prod (Exact s) (Exact s')      = Exact (prod s s')
  prod (Exact s) (Bound l u)     = Bound (prod (Just s) l) (prod (Just s) u)
  prod (Bound l u) (Bound l' u') = Bound (prod l l') (prod u u') -- (prod l u') (prod l' u))
  prod s s'                      = prod s' s

  one  = Exact one
  zero = Exact zero

  isUnit (Exact s) = isUnit s
  isUnit (Bound x y) = isUnit x && isUnit y

instance RegionRig RegionSum where
  prod (Sum ss) (Sum ss') =
   Sum $ nub $ -- Take the cross product of list of summed specifications
     do (Product spec) <- ss
        (Product spec') <- ss'
        return $ Product $ nub $ sort $ spec ++ spec'
  sum (Sum ss) (Sum ss') = Sum $ ss ++ ss'
  zero = Sum []
  one = Sum [Product []]
  isUnit s@(Sum ss) = s == zero || s == one || all (== Product []) ss

-- Pretty print top-level specifications
instance Show Specification where
  show (Specification sp True)  = "stencil " ++ show sp
  show (Specification sp False) = "access " ++ show sp

instance {-# OVERLAPS #-} Show (Multiplicity (Approximation Spatial)) where
  show mult
    | Mult appr <- mult = apprStr empty empty appr
    | Once appr <- mult = apprStr "readOnce" ", " appr
    where
      apprStr linearity sep appr =
        case appr of
          Exact s -> linearity ++ optionalSeparator sep (show s)
          Bound Nothing Nothing -> "empty"
          Bound Nothing (Just s) -> linearity ++ optionalSeparator sep "atMost, " ++ show s
          Bound (Just s) Nothing -> linearity ++ optionalSeparator sep "atLeast, " ++ show s
          Bound (Just sL) (Just sU) ->
            concat [ linearity, optionalSeparator sep (show sL), ";"
                   , if linearity == empty then "" else " " ++ linearity ++ ", "
                   , "atMost, ", show sU ]
      optionalSeparator _   "" = ""
      optionalSeparator sep s  = sep ++ s

instance {-# OVERLAPS #-} Show (Approximation Spatial) where
  show (Exact s) = show s
  show (Bound Nothing Nothing) = "empty"
  show (Bound Nothing (Just s)) = "atMost, " ++ show s
  show (Bound (Just s) Nothing) = "atLeast, " ++ show s
  show (Bound (Just sL) (Just sU)) =
      "atLeast, " ++ show sL ++ "; atMost, " ++ show sU

-- Pretty print spatial specs
instance Show Spatial where
  show (Spatial region) =
    -- Map "empty" spec to Nothing here
    case show region of
      "empty" -> ""
      xs      -> xs

-- Pretty print region sums
instance Show RegionSum where
    showsPrec _ (Sum []) = showString "empty"

    showsPrec p (Sum specs) =
        showParen (p > 6) $ inter specs
      where
        inter [ ] = id
        inter [ x ] = showsPrec 6 x
        inter (x:xs) = showsPrec 6 x . (" + " ++) . inter xs

instance Show RegionProd where
    showsPrec _ (Product []) = showString "empty"
    showsPrec p (Product ss)  =
        showParen (p > 7) $ inter ss
      where
        inter [ ] = id
        inter [ x ] = showsPrec 7 x
        inter (x:xs) = showsPrec 7 x . ('*' :) . inter xs

instance Show Region where
   show (Forward dep dim reflx)   = showRegion "forward" dep dim reflx
   show (Backward dep dim reflx)  = showRegion "backward" dep dim reflx
   show (Centered dep dim reflx)
     | dep == 0 = "pointed(dim=" ++ show dim ++ ")"
     | otherwise = showRegion "centered" dep dim reflx

-- Helper for showing regions
showRegion typ depS dimS reflx = typ ++ "(depth=" ++ show depS
                               ++ ", dim=" ++ show dimS
                               ++ (if reflx then "" else ", nonpointed")
                               ++ ")"

-- Helper for reassociating an association list, grouping the keys together that
-- have matching values
groupKeyBy :: Eq b => [(a, b)] -> [([a], b)]
groupKeyBy = groupKeyBy' . map (\ (k, v) -> ([k], v))
  where
    groupKeyBy' []        = []
    groupKeyBy' [(ks, v)] = [(ks, v)]
    groupKeyBy' ((ks1, v1):((ks2, v2):xs))
      | v1 == v2          = groupKeyBy' ((ks1 ++ ks2, v1) : xs)
      | otherwise         = (ks1, v1) : groupKeyBy' ((ks2, v2) : xs)
