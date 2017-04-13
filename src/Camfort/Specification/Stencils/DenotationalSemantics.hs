{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Camfort.Specification.Stencils.DenotationalSemantics ( intervalsToRegions
                                                            , regionsToIntervals ) where

import Algebra.Lattice
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as SG

import qualified Camfort.Helpers.Vec as V
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.Syntax

-- preconditions:
-- 1. If finite interval, all have "lower bound <= 0 <= upper bound";
-- 2. No dimensionality of 0; (insured by dep. type);
-- 3. All unioned interval lists are of equal length (insured by dep.  type).
intervalsToRegions :: UnionNF (V.S n) (Interval Standard)
                   -> Either String Spatial
intervalsToRegions as = do
    sums <- mapM toProd . NE.toList . SG.sconcat . fmap asymmetryElim $ as
    return . Spatial . Sum $ sums
  where
    asymmetryElim :: V.Vec n (Interval a) -> UnionNF n (Interval a)
    asymmetryElim ints
      | Just ix <- findAsymmetry ints =
        case ints V.!! ix of
          IntervHoled m n p ->
            asymmetryElim (V.replace ix (IntervHoled m 0 p) ints) SG.<>
            asymmetryElim (V.replace ix (IntervHoled 0 n p) ints)
      | otherwise = return ints
    findAsymmetry =
      V.findIndex $ \case
        (IntervHoled m n _) -> m /= 0 && n /= 0 && m /= -n
        _ -> False

    toProd :: V.Vec n (Interval Standard) -> Either String RegionProd
    toProd ints =
      fmap Product $ mapM convert
                   . filter (isNonInf . fst)
                   $ zip (V.toList ints) [0..]

    isNonInf :: Interval Standard -> Bool
    isNonInf IntervInfinite = False
    isNonInf _ = True

    convert :: (Interval Standard, Int) -> Either String Region
    convert (IntervHoled 0 0 False, _) =
      Left "Empty set cannot be realised as a region."
    convert (IntervHoled 0 0 True, ix) = return $ Centered 0 (ix + 1) True
    convert (IntervHoled 0 m p, ix) = return $ Forward (fromIntegral m) (ix + 1) p
    convert (IntervHoled m 0 p, ix) = return $ Backward (fromIntegral $ -m) (ix + 1) p
    convert (IntervHoled m n p, ix) = return $ Centered (fromIntegral n) (ix + 1) p
    convert _ = Left "Infinite interval cannot be realised as a region."

regionsToIntervals :: forall n .
                      V.Natural n
                   -> Spatial
                   -> Either String (UnionNF n (Interval Standard))
regionsToIntervals nOfDims (Spatial (Sum prods))
    | null prods = Left "Empty region sum"
    | otherwise =
      fmap SG.sconcat . sequence . fmap convert . NE.fromList $ prods
  where
    convert :: RegionProd -> Either String (UnionNF n (Interval Standard))
    convert (Product rs)
      | null rs = Left "Empty region product"
      | otherwise = Right $ meets1 . map convert' $ rs

    convert' r = return $ proto nOfDims $
      case r of
        Forward  dep dim p -> (dim-1, IntervHoled 0 (fromIntegral dep) p)
        Backward dep dim p -> (dim-1, IntervHoled (- fromIntegral dep) 0 p)
        Centered dep dim p -> (dim-1, IntervHoled (- fromIntegral dep) (fromIntegral dep) p)

    proto :: forall n .
             V.Natural n
          -> (Int, Interval Standard)
          -> V.Vec n (Interval Standard)
    proto V.Zero _ = V.Nil
    proto (V.Succ n) (i, interv) = V.Cons
      (if i == 0 then interv else IntervInfinite)
      (proto n (i-1, interv))
