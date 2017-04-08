{-# LANGUAGE LambdaCase #-}

module Camfort.Specification.Stencils.DenotationalSemantics ( intervalsToRegions
                                                            , regionsToIntervals ) where

import Control.Monad as CM (join, when)

import Algebra.Lattice
import qualified Data.List.NonEmpty as NE
import Data.List
import qualified Data.Semigroup as SG

import Camfort.Specification.Stencils.LatticeModel
import Camfort.Specification.Stencils.Syntax

sanityCheck :: UnionNF Interval -> Either String ()
sanityCheck union = do
  when (any null union) $
    fail "Dimensionless interval list is observed."
  when (any ((/= dimensionality) . length) union) $
    fail "Dimensionality is not uniform."
  return ()
  where
    dimensionality = length . NE.head $ union

-- preconditions:
-- Intervals are
-- 1. If finite all have lower bound <= 0 <= upper bound;
-- 2. No dimensionality of 0;
-- 3. All unioned interval lists are of equal length.
intervalsToRegions :: UnionNF Interval -> Either String Spatial
intervalsToRegions as = do
    sanityCheck as -- TODO: Delete if too much overhead!
    sums <- mapM toProd . NE.toList . CM.join . NE.map asymmetryElim $ as
    return $ Spatial . Sum $ sums
  where
    asymmetryElim :: [ Interval ] -> UnionNF Interval
    asymmetryElim ints
      | Just ix <- findAsymmetry ints =
        case ints !! ix of
          Interval m n p ->
            asymmetryElim (replace ints ix (Interval m 0 p)) SG.<>
            asymmetryElim (replace ints ix (Interval 0 n p))
      | otherwise = return ints
    findAsymmetry =
      findIndex $ \case
        (Interval m n _) -> m /= 0 && n /= 0 && m /= -n
        _ -> False
    replace :: [ a ] -> Int -> a -> [ a ]
    replace ints ix interv = take ix ints ++ interv : drop (ix + 1) ints

    toProd :: [ Interval ] -> Either String RegionProd
    toProd ints = do
      prods <- mapM convert . collectIntervs $ ints
      return $ Product prods

    collectIntervs :: [ Interval ] -> [ (Interval, Int) ]
    collectIntervs = collectIntervs' 0
    collectIntervs' _ [] = []
    collectIntervs' i (int@Interval{}:ints) = (int,i) : collectIntervs' (i+1) ints

    convert :: (Interval, Int) -> Either String Region
    convert (Interval 0 0 False, _) =
      Left "Empty set cannot be realised as a region."
    convert (Interval 0 m p, ix) = return $ Forward (fromInteger m) ix p
    convert (Interval m 0 p, ix) = return $ Backward (fromInteger $ -m) ix p
    convert (Interval m n p, ix) = return $ Centered (fromInteger n) ix p
    convert _ = Left "Infinite interval cannot be realised as a region."

regionsToIntervals :: Int -> Spatial -> UnionNF Interval
regionsToIntervals nOfDims (Spatial (Sum prods)) =
    foldr1 (SG.<>) . map convert $ prods
  where
    convert :: RegionProd -> UnionNF Interval
    convert (Product []) = error "Empty product"
    convert (Product rs) = foldr1 (/\) . map convert' $ rs
    convert' r = proto $
      case r of
        Forward  dep dim p -> (dim-1, Interval 0 (toInteger dep) p)
        Backward dep dim p -> (dim-1, Interval (- toInteger dep) 0 p)
        Centered dep dim p -> (dim-1, Interval (- toInteger dep) (toInteger dep) p)
    proto (i, interv) =
      return $ take i top ++ interv : take (nOfDims - i - 1) top
    top = replicate nOfDims InfiniteInterval
