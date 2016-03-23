
-- Old inference

{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleContexts, ImplicitParams, TupleSections #-}

module Camfort.Analysis.StencilsForparOldInference where

import Language.Fortran hiding (Spec)

import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Camfort.Analysis.StencilInferenceEngine
import Camfort.Analysis.StencilSpecs
import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations
import Camfort.Extensions.UnitsForpar (parameterise)
import Camfort.Helpers.Vec
import Camfort.Helpers hiding (lineCol, spanLineCol) -- These two are redefined here for ForPar ASTs

import qualified Forpar.AST as F
import qualified Forpar.Analysis as FA
import qualified Forpar.Analysis.Types as FAT
import qualified Forpar.Analysis.Renaming as FAR
import qualified Forpar.Util.Position as FU

import qualified Data.Map as Map
import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe
import Data.List
import Data.Tuple (swap)
import Data.Ord

import Debug.Trace


{- *** 3 . Intermediate representation 'SpecI' between indexing expressions and speccs -}


ixCollectionToSpecOld :: [Variable] -> [[F.Expression A]] -> [Spec]
ixCollectionToSpecOld ivs es = specIsToSpecs . normalise . ixExprAToSpecIs ivs $ es

-- SpecIification (intermediate) elements
data SpecI where
     -- Regular spatial spans
     Span        :: Dimension -> Depth -> Direction -> Saturation -> SpecI
     -- Reflexive access
     Reflx       :: Dimension -> SpecI
     -- Constant access
     Const       :: Dimension -> SpecI
deriving instance Show SpecI

depth :: SpecI -> Int
depth (Span _ depth _ _) = depth
depth x = 0

dim :: SpecI -> Dimension
dim (Span dim _ _ _) = dim
dim (Const dim)      = dim
dim (Reflx dim)      = dim

direction :: SpecI -> Direction
direction (Span _ _ dir _) = dir
direction x                = Fwd

{-
 This provides a representation for index ranges along with
 a normalisation function that coalesces contiguous ranges.

 Any non-reflexive index is converted to a (list of) spans
  e.g. a(i - 1, j + 1) -> [Span 0 1 Bwd False, Span 0 1 Fwd False]

 the 'normalise' function then turns a list of these spans
 into a list of list of spans (per dimension/direction)

-}

-- Ordering
deriving instance Eq SpecI
instance Ord SpecI where
  s1 <= s2 | (dim s1) < (dim s2)  = True
  s1 <= s2 | (dim s1) > (dim s2)  = False
  s1 <= s2 | (dim s1) == (dim s2) =
     case (s1, s2) of
       (Reflx _, _) -> True
       (Const _, _) -> True
       (Span dim depth dir s, Span dim' depth' dir' s')
         | (dim == dim') && (dir == dir') -> depth <= depth'
         | (dim == dim')                  -> dir <= dir'
       (_, _)       -> False

-- Types various normal forms of specifications and specification groups
data Normalised a where
  -- Two specifications belonging to the same dimension which are not duplicates
  NS :: SpecI -> SpecI -> Normalised (SpecI, SpecI)
  -- A list of specifications all of the same dimension
  NSpecIs :: [SpecI]   -> Normalised [SpecI]
  -- Grouped lists of specifications in normal form (maximally coalesced), grouped by dimension
  NSpecIGroups :: [[SpecI]] -> Normalised [[SpecI]]

deriving instance Show (Normalised a)





-- Normalise a list of spans
normalise :: [SpecI] -> Normalised [[SpecI]]
normalise = coalesce . firstAsSaturated . groupByDim

-- Takes lists of specs belonging to the same dimension/direction and coalesces contiguous regions
coalesce :: [Normalised [SpecI]] -> Normalised [[SpecI]]
coalesce = NSpecIGroups . map (\ (NSpecIs specs) -> foldPair (\ x y -> plus (NS x y)) specs)

groupByDim :: [SpecI] -> [Normalised [SpecI]]
groupByDim = map (NSpecIs . nub) . groupBy ((==) `on` dim) . sort

-- Mark spans from 0 to 1 as saturated.
firstAsSaturated :: [Normalised [SpecI]] -> [Normalised [SpecI]]
firstAsSaturated []               = []
firstAsSaturated ((NSpecIs s):xs) = NSpecIs (map go s) : firstAsSaturated xs
  where go (Span dim 1 dir sat) = Span dim 1 dir True
        go s                    = s

-- Coalesces two contiguous specifications (of the same dimension and direction)
--  This is a partial operation and fails when the two specs are not contiguous
plus :: Normalised (SpecI, SpecI) -> Maybe SpecI
plus (NS (Span _ d1 dir s1) (Span dim d2 dir' s2))
  | dir == dir'                                      =
    if d2 == d1 + 1 then -- SpecIs are one apart
       if s1 || s2 then    -- At least one is marked as saturated
           Just (Span dim d2 dir True) -- Grow the saturated area
       else
           Nothing         -- Neither span is satured so mark both as needed
    else -- d2 >= d1 by Normalised -- SpecIs are more than one apart
       if s2 then -- if the greater span is saturated, then it subsumes the smaller
           Just (Span dim d2 dir True)
       else
           Nothing
plus (NS (Const dim1) (Const dim2))                  = Just $ Const dim1 -- assumes Normalised premise
plus (NS s@(Span _ d1 dir s1) (Span dim d2 dir' s2)) = Nothing
plus (NS (Reflx d) (Reflx _))                        = Just $ Reflx d
plus (NS s@(Span _ d1 dir s1) (Reflx _))             = Just $ s
plus (NS  (Reflx _) s@(Span _ d1 dir s1))            = Just $ s
plus _ = error "Trying to coalesce a reflexive and a span"

-- Convert a normalised list of index specifications to a list of specifications
specIsToSpecs :: Normalised [[SpecI]] -> [Spec]
specIsToSpecs x@(NSpecIGroups spanss) = simplify (uncurry go =<< zip [1..] spanss)
  where
    go :: Dimension -> [SpecI] -> [Spec]
    go dim (Reflx _ : xs)            = Reflexive [dim] : go dim xs
    go dim (Const _ : xs)            = Constant [dim] : go dim xs
    go dim [Span _ d Fwd True, Span _ d' Bwd True]
      | d == d'                      = [Symmetric d [dim]]
      | d > d'                       = [Symmetric (abs (d - d')) [dim], Forward d [dim]]
      | otherwise                    = [Symmetric (abs (d - d')) [dim], Backward d' [dim]]
    go dim (Span _ d Fwd True : xs)  = Forward d [dim] : go dim xs
    go dim (Span _ d Bwd True : xs)  = Backward d [dim] : go dim xs
    go dim xs                        = []

    isReflexiveMultiDim :: Normalised [[SpecI]] -> Bool
    isReflexiveMultiDim (NSpecIGroups spanss) = flip all spanss $ \ spans ->
      case spans of (Reflx _) :_  -> True
                    _             -> False

-- From a list of index expressions (themselves a list of expressions)
--  to a set of intermediate specs
ixExprAToSpecIs :: [Variable] -> [[F.Expression A]] -> [SpecI]
ixExprAToSpecIs ivs = concatMap (fromMaybe [] . zipWithM (ixCompExprToSpecI ivs) [0..])

-- Convert a single index expression for a particular dimension to intermediate spec
-- e.g., for the expression a(i+1,j+1) then this function gets
-- passed dim = 0, expr = i + 1 and dim = 1, expr = j + 1
ixCompExprToSpecI :: [Variable] -> Dimension -> F.Expression A -> Maybe SpecI
ixCompExprToSpecI ivs d (F.ExpValue _ _ (F.ValVariable _ v))
  | v `elem` ivs = Just $ Reflx d
  | otherwise    = Just $ Const d
ixCompExprToSpecI ivs d (F.ExpBinary _ _ F.Addition (F.ExpValue _ _ (F.ValVariable _ v))
                                                    (F.ExpValue _ _ (F.ValInteger offs)))
  | v `elem` ivs = Just $ Span d x (if x < 0 then Bwd else Fwd) False
  where x = read offs
ixCompExprToSpecI ivs d (F.ExpBinary _ _ F.Addition (F.ExpValue _ _ (F.ValInteger offs))
                                                    (F.ExpValue _ _ (F.ValVariable _ v)))
  | v `elem` ivs = Just $ Span d x (if x < 0 then Bwd else Fwd) False
  where x = read offs
ixCompExprToSpecI ivs d (F.ExpBinary _ _ F.Subtraction (F.ExpValue _ _ (F.ValVariable _ v))
                                                       (F.ExpValue _ _ (F.ValInteger offs)))
  | v `elem` ivs = Just $ Span d x (if x < 0 then Fwd else Bwd) False
  where x = read offs
ixCompExprToSpecI ivs d (F.ExpValue _ _ (F.ValInteger _)) = Just $ Const d
ixCompExprToSpecI ivs d _ = Nothing
