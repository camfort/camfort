{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Camfort.Specification.Stencils.DenotationalSemantics ( intervalsToRegions
                                                            , regionsToIntervals ) where

import Control.Monad as CM (join, when)

import Algebra.Lattice
import qualified Data.List.NonEmpty as NE
import Data.List
import qualified Data.Semigroup as SG
import Data.Proxy
import qualified Data.Monoid as M

import qualified Camfort.Helpers.Vec as V
import Camfort.Specification.Stencils.LatticeModel
import Camfort.Specification.Stencils.Syntax

-- preconditions:
-- 1. If finite interval, all have "lower bound <= 0 <= upper bound";
-- 2. No dimensionality of 0; (insured by dep. type);
-- 3. All unioned interval lists are of equal length (insured by dep.  type).
intervalsToRegions :: UnionNF (V.S n) Interval -> Either String Spatial
intervalsToRegions as = do
    sums <- mapM toProd . NE.toList . CM.join . NE.map asymmetryElim $ as
    return . Spatial . Sum $ sums
  where
    asymmetryElim :: V.Vec n Interval -> UnionNF n Interval
    asymmetryElim ints
      | Just ix <- findAsymmetry ints =
        case ints V.!! ix of
          Interval m n p ->
            asymmetryElim (V.replace ix (Interval m 0 p) ints) SG.<>
            asymmetryElim (V.replace ix (Interval 0 n p) ints)
      | otherwise = return ints
    findAsymmetry =
      V.findIndex $ \case
        (Interval m n _) -> m /= 0 && n /= 0 && m /= -n
        _ -> False

    toProd :: V.Vec n Interval -> Either String RegionProd
    toProd ints = Product <$> (mapM convert . (`zip` [0..]) . V.toList $ ints)

    convert :: (Interval, Int) -> Either String Region
    convert (Interval 0 0 False, _) =
      Left "Empty set cannot be realised as a region."
    convert (Interval 0 m p, ix) = return $ Forward (fromInteger m) ix p
    convert (Interval m 0 p, ix) = return $ Backward (fromInteger $ -m) ix p
    convert (Interval m n p, ix) = return $ Centered (fromInteger n) ix p
    convert _ = Left "Infinite interval cannot be realised as a region."

regionsToIntervals :: forall n . V.Natural n -> Spatial -> Either String (UnionNF n Interval)
regionsToIntervals nOfDims (Spatial (Sum prods))
    | null prods = Left "Empty region sum"
    | otherwise = SG.sconcat . fmap convert . NE.fromList $ prods
  where
    convert :: RegionProd -> Either String (UnionNF n Interval)
    convert (Product rs)
      | null rs = Left "Empty region product"
      | otherwise = Right $ joins1 . map convert' $ rs

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
