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

import Debug.Trace

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

-- High-level spec algebra
data Spec where
     Reflexive :: Spec
     Forward :: Depth -> [Dimension] -> Spec
     Backward :: Depth -> [Dimension] -> Spec
     Symmetric :: Depth -> [Dimension] -> Spec
     Unspecified :: [Dimension] -> Spec
     Linear :: Spec -> Spec

-- Syntax
showL = concat . (intersperse ",") . (map show)
instance Show Spec where
     show Reflexive            = "reflexive"
     show (Forward dep dims)   = "forward " ++ show dep ++ " " ++ showL dims
     show (Backward dep dims)  = "backward " ++ show dep ++ " " ++ showL dims
     show (Symmetric dep dims) = "centered " ++ show dep ++ " " ++ showL dims
     show (Unspecified dims)   = "unspecified " ++ showL dims
     show (Linear spec)        = (show spec) ++ " unique "

--specGen :: Normalised [[SpecI]] -> [Spec]
--specGen =

isReflexiveMultiDim :: Normalised [[SpecI]] -> Bool
isReflexiveMultiDim (NSpecIGroups spanss) = all (\spans -> (length spans > 0) && (head spans == Reflx)) spanss

-- Find speccs for the same depth and direction but differnt dim and combine them
coalesceSameDepth :: [Spec] -> [Spec]
coalesceSameDepth = id 

specIsToSpecs :: Normalised [[SpecI]] -> [Spec]
specIsToSpecs x@(NSpecIGroups spanss) =
     (if isReflexiveMultiDim x then [Reflexive] else [])
  ++ coalesceSameDepth (concatMap (uncurry go) (zip [0..length spanss] spanss))
                  where go :: Dimension -> [SpecI] -> [Spec]
                        go dim (Reflx : xs) = go dim xs
                        go dim [Span d _ Fwd True, Span d' _ Bwd True] =
                           if d==d' then [Symmetric d [dim]]
                           else if d > d' then [Symmetric (abs (d-d')) [dim], Forward d [dim]]
                                          else [Symmetric (abs (d-d')) [dim], Backward d' [dim]]
                        go dim xs = (show (dim, xs)) `trace` []
                           

-- SpecIification (intermediate) elements
data SpecI where
     Span        :: Depth -> Dimension -> Direction -> Saturation -> SpecI
     Reflx   :: SpecI
deriving instance Show SpecI

depth :: SpecI -> Int
depth Reflx = 0
depth (Span depth _ _ _) = depth

dim :: SpecI -> Dimension
dim Reflx = 0
dim (Span _ dim _ _) = dim

direction :: SpecI -> Direction
direction (Span _ _ dir _) = dir
direction Reflx        = Fwd

ixExprToSpecIs :: Expr p -> [SpecI]
ixExprToSpecIs (Var _ _ [(VarName _ a, es)]) | length es > 0 =
              case (mapM (uncurry ixCompExprToSpecI) (zip [0..(length es)] es)) of
                   Nothing -> []
                   Just es -> es
ixExprToSpecIs _ = []                   

ixCompExprToSpecI :: Dimension -> Expr p -> Maybe SpecI
ixCompExprToSpecI d (Var _ _ [(VarName _ v, [])]) = Just Reflx

ixCompExprToSpecI d (Bin _ _ (Plus _) (Var _ _ [(VarName _ v, [])]) (Con _ _ offset)) =
                       let x = read offset in Just $ Span (read offset) d (if x < 0 then Bwd else Fwd) False

ixCompExprToSpecI d (Bin _ _ (Plus _) (Con _ _ offset) (Var _ _ [(VarName _ v, [])])) =
                       let x = read offset in Just $ Span (read offset) d (if x < 0 then Bwd else Fwd) False

ixCompExprToSpecI d (Bin _ _ (Minus _) (Var _ _ [(VarName _ v, [])]) (Con _ _ offset)) =
                       let x = read offset in Just $ Span (read offset) d (if x < 0 then Fwd else Bwd) False

ixCompExprToSpecI d _ = Nothing


{-
 This provides a representation for index ranges along with
 a normalisation function that coalesces contiguous ranges.

 Any non-reflexive index is converted to a (list of) spans
  e.g. a(i - 1, j + 1) -> [Span 1 Bwd 0 False, Span 1 Fwd 1 False]

 the 'normalise' function then turns a list of these spans 
 into a list of list of spans (per dimension/direction)
 
-}

-- Ordering
deriving instance Eq SpecI
instance Ord SpecI where
         (Span depth dim Fwd s) <= (Span depth' dim' Fwd s')
                  | (dim < dim')  = True
                  | (dim == dim') = depth <= depth'
                  | otherwise     = False
         (Span depth dim Bwd s) <= (Span depth' dim' Bwd s')
                  | (dim <= dim') = True
                  | (dim == dim') = depth <= depth'
                  | otherwise     = False
         (Span _ _ Fwd _) <= (Span _ _ Bwd _) = True
         Reflx       <= _                 = True
         _ <= _                               = False

-- Types various normal forms of specifications and specification groups
data Normalised a where
     -- Two specifications belonging to the same dimension which are not duplicates
     NS :: SpecI -> SpecI -> Normalised (SpecI, SpecI)

     -- A list of specifications all of the same dimension
     NSpecIs :: [SpecI]   -> Normalised [SpecI]

     -- Grouped lists of specifications in normal form (maximally coalesced), grouped by dimension
     NSpecIGroups :: [[SpecI]] -> Normalised [[SpecI]]

-- Normalise a list of spans
normalise :: [SpecI] -> Normalised [[SpecI]]
normalise = coalesce . groupByDim

-- Takes lists of specs belonging to the same dimension/direction and coalesces contiguous regions
coalesce :: [Normalised [SpecI]] -> Normalised [[SpecI]]
coalesce = NSpecIGroups . (map (\(NSpecIs specs) -> foldPair (\x y -> plus (NS x y)) $ specs))

groupByDim :: [SpecI] -> [Normalised [SpecI]]
groupByDim = (map (NSpecIs . nub)) . (groupBy eqDim) . sort

eqDim :: SpecI -> SpecI -> Bool
eqDim Reflx Reflx                 = True
eqDim (Span _ d dir _) (Span _ d' dir' _) = (d == d') && (dir == dir')
eqDim _ _                                 = False

-- Coalesces two contiguous specifications (of the same dimension and direction)
--  This is a partial operation and fails when the two specs are not contiguous
plus :: Normalised (SpecI, SpecI) -> Maybe SpecI
plus (NS (Span d1 _ _ s1) (Span d2 dim dir s2)) =
     if d2 == (d1 + 1) then -- SpecIs are one apart
        if s1 || s2 then    -- At least one is marked as saturated
            Just (Span d2 dim dir True) -- Grow the saturated area
        else 
            Nothing         -- Neither span is satured so mark both as needed
     else -- d2 >= d1 by Normalised -- SpecIs are more than one apart
        if s2 then -- if the greater span is saturated, then it subsumes the smaller
            Just (Span d2 dim dir True)
        else
            Nothing
plus (NS Reflx Reflx) = Just Reflx
plus _ = error "Trying to coalesce a reflexive and a span"

-- Helper function, reduces a list two elements at a time with a partial operation
foldPair :: (a -> a -> Maybe a) -> [a] -> [a]
foldPair f [] = []
foldPair f [a] = [a]
foldPair f (a:(b:xs)) = case f a b of
                          Nothing -> a : (foldPair f (b : xs))
                          Just c  -> foldPair f (c : xs)
