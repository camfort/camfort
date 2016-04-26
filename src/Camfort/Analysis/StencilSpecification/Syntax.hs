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

{-# LANGUAGE TypeOperators, DataKinds, DeriveDataTypeable, FlexibleInstances #-}

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
--  products on dimensions and unions):
--    i.e., (A * B) U (C * D)... 
data SpatialSpec = 
    SpatialSpec { irreflexives :: [Dimension],
                  reflexives   :: [Dimension],
                  spatial      :: SpecUnion }
  deriving (Eq, Data, Typeable)

emptySpec = SpatialSpec [] [] (Union [Product []])

type Dimension  = Int -- spatial dimensions are 1 indexed
type Depth      = Int

-- The inner elements of a specification
data Spec =
    Forward   Depth [Dimension] 
  | Backward  Depth [Dimension]
  | Symmetric Depth [Dimension]
  | Constant  [Dimension]
  deriving (Eq, Data, Typeable)

-- Product of specifications
newtype SpecProd = Product [Spec]
  deriving (Eq, Data, Typeable)
  
-- Union of product specifications
newtype SpecUnion = Union [SpecProd]
  deriving (Eq, Data, Typeable)

injectSpec ss = SpatialSpec [] [] (Union ss)

-- An (arbitrary) ordering on specifications for the sake of normalisation
instance Ord Spec where
  (Forward dep dims) <= (Forward dep' dims') 
    | dep == dep' = (sort dims) <= (sort dims')
    | otherwise   = dep <= dep'

  (Backward dep dims) <= (Backward dep' dims') 
    | dep == dep' = (sort dims) <= (sort dims')
    | otherwise   = dep <= dep'

  (Symmetric dep dims) <= (Symmetric dep' dims') 
    | dep == dep' = (sort dims) <= (sort dims')
    | otherwise   = dep <= dep'

  (Constant dims) <= (Constant dims') =
    (sort dims) <= (sort dims')

  -- Order in the way defined above: Forward <: Backward <: Symmetric <: Constant
  (Forward _ _ ) <= _               = True
  (Backward _ _) <= (Symmetric _ _) = True
  _              <= (Constant _)    = True
  _              <= _               = False

instance Ord SpecProd where
   (Product xs) <= (Product xs')
     | length xs == length xs' = xs <= xs'
     | otherwise               = (length xs) <= (length xs')

{-

instance Ord Specification where
     Empty           <= _                = True
     _               <= Empty            = False
     
     (Reflexive ds)  <= (Reflexive ds')  = ds <= ds'
     (Reflexive ds)  <= _                = True
     _               <= (Reflexive ds)   = False
     
     (Forward depth ds)  <= (Forward depth' ds')
       | ds == ds' = depth <= depth'
       | otherwise = ds <= ds'
       
     (Backward depth ds) <= (Backward depth' ds')
       | ds == ds' = depth <= depth'
       | otherwise = ds <= ds'
       
     (Symmetric depth ds) <= (Symmetric depth' ds')
       | ds == ds' = depth <= depth'
       | otherwise = ds <= ds'
       
     (Product specs)  <= (Product specs')  = specs <= specs'
     (Irrefl ds spec) <= (Irrefl ds' spec')
       | ds = ds' = spec <= spec'
       | otherwise = ds <= ds'
     
     (TemporalFwd vs) <= (TemporalFwd vs') = vs <= vs'
     (TemporalBwd vs) <= (TemporalBwd vs') = vs <= vs'
     (Unspecified ds) <= (Unspecified ds') = ds <= ds'
     (Constant ds)    <= (Constant ds')    = ds <= ds'
     (Linear s)       <= (Linear s')       = s <= s'
     -- Otherwise do lexicographic ordering on the pretty printed output
     s                <= s'                = (head $ show s) <= (head $ show s')

-}

-- `specPlus` combines specs by coalescing specifications with the same
-- direction and depth into one
--   e.g. forward, depth=1, dims=1 `specPlus` forward, depth=1,dims=2
--       = forward, depth=1, dims=1,2

instance PartialMonoid Spec where

  emptyM = Constant []
  
  appendM (Forward dep dims) (Forward dep' dims')
    | dep == dep' = Just $ Forward dep (sort $ dims ++ dims')
  appendM (Backward dep dims) (Backward dep' dims')
    | dep == dep' = Just $ Backward dep (sort $ dims ++ dims')
  appendM (Symmetric dep dims) (Symmetric dep' dims')
    | dep == dep' = Just $ Symmetric dep (sort $ dims ++ dims')
  appendM (Constant dims) (Constant dims') =
    Just $ Constant (sort $ dims ++ dims')
  appendM (Constant []) x = Just x
  appendM x (Constant []) = Just x
  appendM x y             = Nothing


{-
unionSpec :: Specification -> Specification -> Specification
unionSpec Empty x = x
unionSpec x Empty = x
unionSpec (Linear ss) (Linear ss')       = inferLinearity $ unionSpatialSpec ss ss'
unionSpec (Linear ss) (NonLinear ss')    = NonLinear $ unionSpatialSpec ss ss'
unionSpec (NonLinear ss) (Linear ss')    = NonLinear $ unionSpatialSpec ss ss'
unionSpec (NonLinear ss) (NonLinear ss') = NonLinear $ unionSpatialSpec ss ss'
-}

instance PartialMonoid SpecProd where
   emptyM = Product []

   appendM (Product [])   s  = Just $ s
   appendM s (Product [])    = Just $ s
   appendM (Product [ss]) (Product [ss']) = appendM ss ss' >>= (\ss'' -> return $ Product [ss''])
   appendM _              _  = Nothing


unionSpatialSpec :: SpatialSpec -> SpatialSpec -> SpatialSpec
unionSpatialSpec (SpatialSpec irdim rdim (Union ss)) (SpatialSpec irdim' rdim' (Union ss')) =
    SpatialSpec (irdim ++ irdim') (rdim ++ rdim') (Union $ normalise $ ss ++ ss')

prodSpatialSpec :: SpatialSpec -> SpatialSpec -> SpatialSpec
prodSpatialSpec (SpatialSpec irdim rdim s) (SpatialSpec irdim' rdim' s') =
    SpatialSpec (irdim ++ irdim') (rdim ++ rdim') (prodSpecUnion s s')

prodSpecUnion :: SpecUnion -> SpecUnion -> SpecUnion
prodSpecUnion (Union ss) (Union ss') =
   Union $ -- Take the cross product of list of unioned specifications
           do (Product spec) <- ss
              (Product spec') <- ss'
              return $ Product $ normalise $ spec ++ spec'

-- Show a list with ',' separator (used to represent union of regions)
showL :: Show a => [a] -> String
showL = concat . (intersperse ",") . (map show)

showUnionSpecs :: Show a => [a] -> String
showUnionSpecs = showL

-- Show a list with '*' separator (used to represent product of regions)
showProdSpecs :: Show a => [a] -> String
showProdSpecs = concat . (intersperse "*") . (map show)

-- Pretty-printed syntax

instance Show Specification where
    show (Linear spec)        = (show spec) ++ ", read-once"
    show (NonLinear spec)     = show spec
    show Empty                = "none"
    show (TemporalFwd tdims)  = showRegion "forward" (show $ length tdims) ("t{" ++ showL tdims ++ "}")
    show (TemporalBwd tdims)  = showRegion "backward" (show $ length tdims) ("t{" ++ showL tdims ++ "}")

instance Show SpatialSpec where
    -- Tweedle-dum
    show (SpatialSpec [] [] (Union [])) = "none"
    -- Tweedle-dee
    show (SpatialSpec [] [] (Union [Product []])) = "none"

    show (SpatialSpec irdims rdims (Union specs)) =
      concat $ intersperse ", " ppspecs
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
       concat . (intersperse "*") . (map ((\s -> "(" ++ showInsideProd s ++ ")"))) $ ss

showInsideProd (Forward dep dims)   = showRegion "forward" (show dep) (showProdSpecs dims) 
showInsideProd (Backward dep dims)  = showRegion "backward" (show dep) (showProdSpecs dims)
showInsideProd (Symmetric dep dims) = showRegion "symmetry" (show dep) (showProdSpecs dims)
showInsideProd (Constant dims)      = "fixed, " ++ (showProdSpecs dims)

-- Helper for showing regions
showRegion typ depS dimS = typ ++ ", depth=" ++ depS ++ ", dim=" ++ dimS

instance Show Spec where
    show (Forward dep dims)   = showRegion "forward" (show dep) (showL dims)
    show (Backward dep dims)  = showRegion "backward" (show dep) (showL dims)
    show (Symmetric dep dims) = showRegion "centered" (show dep) (showL dims)
    show (Constant dims)      = "fixed, dim=" ++ showL dims
    





