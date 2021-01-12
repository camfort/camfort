{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wall #-}

{-|
For expressions over normal Fortran values that are not representable in
Fortran.

- Immutable array update ('MopWriteArr')
- Immutable data update ('MopWriteData')
- Explicit coercions ('MopCoercePrim')
-}

module Language.Fortran.Model.Op.Meta (MetaOp(..)) where

import           Data.Functor.Compose

import           Data.Vinyl                          (Rec, RMap, RApply, rmap, (<<*>>))
import           Data.Vinyl.Functor                  (Lift (..))
import           Data.Vinyl.Lens                     (RElem, rput)

import           Data.Singletons.TypeLits

import qualified Data.SBV.Dynamic                    as SBV

import           Language.Expression
import           Language.Expression.Pretty

import           Language.Fortran.Model.Op.Core.Eval
import           Language.Fortran.Model.Op.Eval
import           Language.Fortran.Model.Repr
import           Language.Fortran.Model.Types


data MetaOp t a where
  MopWriteArr
    :: D (Array i v)
    -> t (Array i v)
    -> t i
    -> t v
    -> MetaOp t (Array i v)

  {-|

  In @'MopWriteData' recD fSymb valD recVal valVal@:

  * @recD@ is the type of the record we're writing to.
  * @fSymb@ is the name of the field we're writing to.
  * @valD@ is the type of the value we're writing to.
  * @recVal@ is the original value of the record.
  * @valVal@ is the new value of the field to write to.
  -}
  MopWriteData
    :: RElem '(fname, a) fields i
    => D (Record rname fields)
    -> SSymbol fname
    -> D a
    -> t (Record rname fields)
    -> t a
    -> MetaOp t (Record rname fields)

  MopCoercePrim
    :: Prim p k b
    -> t (PrimS a)
    -> MetaOp t (PrimS b)


instance HFunctor MetaOp where
instance HTraversable MetaOp where
  htraverse f = \case
    MopWriteArr d x y z -> MopWriteArr d <$> f x <*> f y <*> f z
    MopWriteData a b c x y -> MopWriteData a b c <$> f x <*> f y
    MopCoercePrim p x -> MopCoercePrim p <$> f x


instance (MonadEvalFortran r m) => HFoldableAt (Compose m CoreRepr) MetaOp where
  hfoldMap = implHfoldMapCompose $ \case
    MopWriteArr _ arr ix val -> pure $ writeArray arr ix val
    MopWriteData _ fname _ rec val -> pure $ writeDataAt fname rec val
    MopCoercePrim p x -> coercePrim p x


instance (MonadEvalFortran r m) => HFoldableAt (Compose m HighRepr) MetaOp where
  hfoldMap = implHfoldMapCompose $ fmap HRCore . hfoldA .
    hmap (\case HRCore x -> x
                HRHigh _ -> error "impossible")


instance Pretty2 MetaOp where
  prettys2Prec p = \case
    MopWriteArr _ arr i v ->
      -- e.g. @myArrayVar[9 <- "new value"]@
      showParen (p > 9) $ prettys1Prec 10 arr .
                          showString "[" . prettys1Prec 0 i .
                          showString " <- " . prettys1Prec 0 v .
                          showString "]"
    MopWriteData _ fname _ r v ->
      showParen (p > 9) $ prettys1Prec 10 r .
      showString "{" .
      showString (withKnownSymbol fname (symbolVal fname)) .
      showString " <- " .
      prettys1Prec 0 v .
      showString "}"

    -- TODO: Consider adding visual evidence of coercion
    MopCoercePrim _ x -> prettys1Prec p x


--------------------------------------------------------------------------------
--  Write array
--------------------------------------------------------------------------------

rzip3With
  :: (RMap xs, RApply xs)
  => (forall x. f x -> g x -> h x -> i x)
  -> Rec f xs
  -> Rec g xs
  -> Rec h xs
  -> Rec i xs
rzip3With f x y z = rmap (Lift . (Lift .) . f) x <<*>> y <<*>> z

writeArray' :: CoreRepr i -> D (Array i v) -> ArrRepr i v -> CoreRepr v -> ArrRepr i v
writeArray' ixRep (DArray ixIndex@(Index _) valAV) arrRep valRep =
  case ixRep of
    CRPrim _ ixVal -> case (valAV, arrRep, valRep) of
      (ArrPrim _, ARPrim arr, CRPrim _ valVal) -> ARPrim (SBV.writeSArr arr ixVal valVal)
      (ArrData _ fieldsAV, ARData fieldsAR, CRData _ fieldsRep) ->
        ARData (rzip3With (zip3FieldsWith (writeArray' ixRep . DArray ixIndex))
                fieldsAV
                fieldsAR
                fieldsRep)


writeArray :: CoreRepr (Array i v) -> CoreRepr i -> CoreRepr v -> CoreRepr (Array i v)
writeArray (CRArray arrD arrRep) ixRep valRep =
  CRArray arrD (writeArray' ixRep arrD arrRep valRep)

--------------------------------------------------------------------------------
--  Write Data
--------------------------------------------------------------------------------

writeDataAt
  :: RElem '(fname, a) fields i
  => SSymbol fname
  -> CoreRepr (Record rname fields)
  -> CoreRepr a
  -> CoreRepr (Record rname fields)
writeDataAt fieldSymbol (CRData d dataRec) valRep =
  CRData d $ rput (Field fieldSymbol valRep) dataRec

--------------------------------------------------------------------------------
--  Coerce primitives
--------------------------------------------------------------------------------

coercePrim
  :: (MonadEvalFortran r m)
  => Prim p k b
  -> CoreRepr (PrimS a)
  -> m (CoreRepr (PrimS b))
coercePrim prim2 (CRPrim _ v) = CRPrim (DPrim prim2) <$> coercePrimSVal prim2 v
