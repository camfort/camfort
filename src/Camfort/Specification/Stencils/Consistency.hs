{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camfort.Specification.Stencils.Consistency ( consistent
                                                  , ConsistencyResult(..) ) where

import qualified Camfort.Helpers.Vec as V
import Camfort.Specification.Stencils.DenotationalSemantics
import Camfort.Specification.Stencils.LatticeModel
import Camfort.Specification.Stencils.Syntax

data ConsistencyResult =
    Consistent
  | Inconsistent String
  deriving (Eq, Show)

-- | This function checks multiplicity consistency and then delegates the
-- spatial consistency to |consistent'| function.
consistent :: forall n .
              Specification
           -> Multiplicity (UnionNF n Offsets)
           -> IO ConsistencyResult
consistent (Specification mult) observedIxs =
    -- First do the linearity check
    case (specModel, observedIxs) of
      (Mult a, Mult b) -> a `consistent'` b
      (Once a, Once b) -> a `consistent'` b
      (Once _, Mult _) ->return $ Inconsistent
        "Specification is readOnce, but there are repeated indices."
      (Mult _, Once _) -> return $ Inconsistent
        "Specification lacks readOnce, but the indices are inuque."
  where
    specModel :: Multiplicity (Approximation (UnionNF n (Interval Standard)))
    specModel =
      case sequence $ (sequence . fmap (regionsToIntervals nOfDims)) <$> mult of
        Right model -> model
        Left msg -> error msg

    nOfDims :: V.Natural n
    nOfDims = vecLength . peel $ observedIxs

-- | This is the actual consistency check using set comparison supplied in
-- the model.
consistent' :: Approximation (UnionNF n (Interval Standard))
            -> UnionNF n Offsets
            -> IO ConsistencyResult
consistent' (Exact unf) ixs = do
  ord <- ioCompare unf ixs
  return $ case ord of
    EQ -> Consistent
    LT ->
      Inconsistent "The specification covers a smaller area than the indices."
    GT ->
      Inconsistent "The specification covers a larger area than the indices."
consistent' (Bound (Just unf) Nothing) ixs = do
  ord <- ioCompare unf ixs
  return $ case ord of
    EQ -> Consistent
    LT -> Consistent
    GT -> Inconsistent $
      "There are indices covered by the lower bound specification, but " ++
      "could not observed in the indices."
consistent' (Bound Nothing (Just unf)) ixs = do
  ord <- ioCompare unf ixs
  return $ case ord of
    EQ -> Consistent
    GT -> Consistent
    LT -> Inconsistent
      "There are indices outside the upper bound specification."
consistent' (Bound lb ub) ixs = do
  cLower <- Bound lb Nothing `consistent'` ixs
  cUpper <- Bound Nothing ub `consistent'` ixs
  return $ case (cLower, cUpper) of
    (Consistent, Consistent) -> Consistent
    (Consistent, inconsistent) -> inconsistent
    (inconsistent, Consistent) -> inconsistent
    (Inconsistent{}, Inconsistent{}) -> Inconsistent
      "Neither the lower nor ther upper bound conform with the indices."
