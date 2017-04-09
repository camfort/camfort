{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camfort.Specification.Stencils.DenotationalSemantics ( intervalsToRegions
                                                            , regionsToIntervals ) where

import Control.Monad as CM (join, when)

import Algebra.Lattice
import qualified Data.List.NonEmpty as NE
import Data.List
import qualified Data.Semigroup as SG
import Data.Proxy

import qualified Camfort.Helpers.Vec as V
import Camfort.Specification.Stencils.LatticeModel
import Camfort.Specification.Stencils.Syntax

sanityCheck :: UnionNF n Interval -> Either String ()
sanityCheck union = do
  when (any null union) $
    fail "Dimensionless interval list is observed."
  return ()
  where
    dimensionality = length . NE.head $ union

-- preconditions:
-- Intervals are
-- 1. If finite all have lower bound <= 0 <= upper bound;
-- 2. No dimensionality of 0;
-- 3. All unioned interval lists are of equal length (insured by dependent
--    type).
intervalsToRegions :: UnionNF n Interval -> Either String Spatial
intervalsToRegions as = do
    sanityCheck as -- TODO: Delete if too much overhead!
    sums <- mapM toProd . NE.toList . CM.join . NE.map asymmetryElim $ as
    return $ Spatial . Sum $ sums
  where
    asymmetryElim :: V.Vec n Interval -> UnionNF n Interval
    asymmetryElim ints
      | ints' <- V.toList ints
      , Just ix <- findAsymmetry ints' =
        case ints' !! ix of
          Interval m n p ->
            asymmetryElim (replace ix (Interval m 0 p) ints) SG.<>
            asymmetryElim (replace ix (Interval 0 n p) ints)
      | otherwise = return ints
    findAsymmetry =
      findIndex $ \case
        (Interval m n _) -> m /= 0 && n /= 0 && m /= -n
        _ -> False

    replace :: Int -> a -> V.Vec n a -> V.Vec n a
    replace i a = V.applyListOp (replace' i a)

    replace' :: Int -> a -> [ a ] -> [ a ]
    replace' ix interv ints = take ix ints ++ interv : drop (ix + 1) ints

    toProd :: V.Vec n Interval -> Either String RegionProd
    toProd ints = do
      prods <- mapM convert . (`zip` [0..]) . V.toList $ ints
      return $ Product prods

    convert :: (Interval, Int) -> Either String Region
    convert (Interval 0 0 False, _) =
      Left "Empty set cannot be realised as a region."
    convert (Interval 0 m p, ix) = return $ Forward (fromInteger m) ix p
    convert (Interval m 0 p, ix) = return $ Backward (fromInteger $ -m) ix p
    convert (Interval m n p, ix) = return $ Centered (fromInteger n) ix p
    convert _ = Left "Infinite interval cannot be realised as a region."

regionsToIntervals :: forall n . V.IsNatural n
                   => V.Natural n -> Spatial -> UnionNF n Interval
regionsToIntervals nOfDims (Spatial (Sum prods)) =
    foldr1 (SG.<>) . map convert $ prods
  where
    nOfDims' = V.fromNat (Proxy :: Proxy n)

    convert :: RegionProd -> UnionNF n Interval
    convert (Product []) = error "Empty product"
    convert (Product rs) = foldr1 (/\) . map convert' $ rs

    convert' r = return $ proto nOfDims 0 $
      case r of
        Forward  dep dim p -> (dim-1, Interval 0 (toInteger dep) p)
        Backward dep dim p -> (dim-1, Interval (- toInteger dep) 0 p)
        Centered dep dim p -> (dim-1, Interval (- toInteger dep) (toInteger dep) p)

    proto :: forall n
           . V.Natural n -> Int -> (Int, Interval) -> V.Vec n Interval
    proto V.Zero _ _ = V.Nil
    proto (V.Succ n) c p@(i, interv) = V.Cons
      (if c == i then interv else InfiniteInterval)
      (proto n (c+1) p)
