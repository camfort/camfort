{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Analysis.Stencils where

import Language.Fortran hiding (Spec)

import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy

import Analysis.Loops
import Analysis.LVA
import Analysis.Annotations
import Analysis.Syntax
import Analysis.Types

import Data.List
import Helpers
import Traverse

import Transformation.Syntax

infer :: Program a -> Program Annotation
infer p = map ((descendBi arrayIndices) . ix . lvaOnUnit . (transformBi reassociate) . (fmap (const unitAnnotation))) p

check :: Program a -> Program Annotation
check = undefined

{- Algebra of specifications -}

type Dimension  = Int
type Depth      = Int
type Saturation = Bool
data Direction  = Fwd | Bwd deriving (Eq, Show)

-- Specification elements
data Spec where
     Span        :: Depth -> Dimension -> Direction -> Saturation -> Spec
     Reflexive   :: Spec
deriving instance Show Spec

depth :: Spec -> Int
depth Reflexive = 0
depth (Span depth _ _ _) = depth

dim :: Spec -> Dimension
dim Reflexive = 0
dim (Span _ dim _ _) = dim

direction :: Spec -> Direction
direction (Span _ _ dir _) = dir
direction Reflexive        = Fwd


{-
 This provides a representation for index ranges along with
 a normalisation function that coalesces contiguous ranges.

 Any non-reflexive index is converted to a (list of) spans
  e.g. a(i - 1, j + 1) -> [Span 1 Bwd 0 False, Span 1 Fwd 1 False]

 the 'normalise' function then turns a list of these spans 
 into a list of list of spans (per dimension/direction)
 
-}

-- Ordering
deriving instance Eq Spec
instance Ord Spec where
         (Span depth dim Fwd s) <= (Span depth' dim' Fwd s')
                  | (dim < dim')  = True
                  | (dim == dim') = depth <= depth'
                  | otherwise     = False
         (Span depth dim Bwd s) <= (Span depth' dim' Bwd s')
                  | (dim <= dim') = True
                  | (dim == dim') = depth <= depth'
                  | otherwise     = False
         (Span _ _ Fwd _) <= (Span _ _ Bwd _) = True
         Reflexive       <= _                 = True
         _ <= _                               = False

-- Types various normal forms of specifications and specification groups
data Normalised a where
     -- Two specifications belonging to the same dimension which are not duplicates
     NS :: Spec -> Spec -> Normalised (Spec, Spec)

     -- A list of specifications all of the same dimension
     NSpecs :: [Spec]   -> Normalised [Spec]

     -- Grouped lists of specifications in normal form (maximally coalesced), grouped by dimension
     NSpecGroups :: [[Spec]] -> Normalised [[Spec]]

-- Normalise a list of spans
normalise :: [Spec] -> Normalised [[Spec]]
normalise = coalesce . groupByDim

-- Takes lists of specs belonging to the same dimension/direction and coalesces contiguous regions
coalesce :: [Normalised [Spec]] -> Normalised [[Spec]]
coalesce = NSpecGroups . (map (\(NSpecs specs) -> foldPair (\x y -> plus (NS x y)) $ specs))

groupByDim :: [Spec] -> [Normalised [Spec]]
groupByDim = (map (NSpecs . nub)) . (groupBy eqDim) . sort

eqDim :: Spec -> Spec -> Bool
eqDim Reflexive Reflexive                 = True
eqDim (Span _ d dir _) (Span _ d' dir' _) = (d == d') && (dir == dir')
eqDim _ _                                 = False

-- Coalesces two contiguous specifications (of the same dimension and direction)
--  This is a partial operation and fails when the two specs are not contiguous
plus :: Normalised (Spec, Spec) -> Maybe Spec
plus (NS (Span d1 _ _ s1) (Span d2 dim dir s2)) =
     if d2 == (d1 + 1) then -- Specs are one apart
        if s1 || s2 then    -- At least one is marked as saturated
            Just (Span d2 dim dir True) -- Grow the saturated area
        else 
            Nothing         -- Neither span is satured so mark both as needed
     else -- d2 >= d1 by Normalised -- Specs are more than one apart
        if s2 then -- if the greater span is saturated, then it subsumes the smaller
            Just (Span d2 dim dir True)
        else
            Nothing
plus (NS Reflexive Reflexive) = Just Reflexive
plus _ = error "Trying to coalesce a reflexive and a span"

-- Helper function, reduces a list two elements at a time with a partial operation
foldPair :: (a -> a -> Maybe a) -> [a] -> [a]
foldPair f [] = []
foldPair f [a] = [a]
foldPair f (a:(b:xs)) = case f a b of
                          Nothing -> a : (foldPair f (b : xs))
                          Just c  -> foldPair f (c : xs)
