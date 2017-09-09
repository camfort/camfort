{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wall #-}

{-|
For expressions over normal Fortran values that are not representable in
Fortran.

- Immutable array update ('MopWriteArr')
- Immutable data update ('MopWriteData')
- Explicit coercions ('MopCoercePrim')
-}
module Language.Fortran.Model.Op.Meta where

import           Data.Vinyl.Lens

import           Data.Singletons.TypeLits

import qualified Data.SBV.Dynamic                    as SBV

import           Language.Expression
import           Language.Expression.Pretty

import           Language.Fortran.Model.Op.Core.Eval
import           Language.Fortran.Model.Repr
import           Language.Fortran.Model.Types


data MetaOp t a where
  MopWriteArr
    :: D (Array i v)
    -> t (Array i v)
    -> t i
    -> t v
    -> MetaOp t (Array i v)

  MopWriteData
    :: RElem '(fname, a) fields i
    => D (Record rname fields) -- ^ Record to write to
    -> SSymbol fname           -- ^ Field to write
    -> D a                     -- ^ New value
    -> t (Record rname fields)
    -> t a
    -> MetaOp t (Record rname fields)

  MopCoercePrim
    :: Prim p k a
    -> Prim p k b
    -> t (PrimS a)
    -> MetaOp t (PrimS b)


instance Operator MetaOp where
  htraverseOp f = \case
    MopWriteArr d x y z -> MopWriteArr d <$> f x <*> f y <*> f z
    MopWriteData a b c x y -> MopWriteData a b c <$> f x <*> f y
    MopCoercePrim p1 p2 x -> MopCoercePrim p1 p2 <$> f x


instance (Applicative f) => EvalOp f CoreRepr MetaOp where
  evalOp = \case
    MopWriteArr _ arr ix val -> pure $ writeArray arr ix val
    MopWriteData _ fname _ rec val -> pure $ writeDataAt fname rec val
    MopCoercePrim p1 p2 x -> pure $ coercePrim p1 p2 x


instance (MonadEvalFortran r m) => EvalOp m HighRepr MetaOp where
  evalOp = fmap HRCore . evalOp .
    hmapOp (\case
               HRCore x -> x
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
    MopCoercePrim _ _ x -> prettys1Prec p x


--------------------------------------------------------------------------------
--  Write array
--------------------------------------------------------------------------------

writeArray :: CoreRepr (Array i v) -> CoreRepr i -> CoreRepr v -> CoreRepr (Array i v)
writeArray arrRep ixRep valRep =
  case arrRep of
    CRArray d@(DArray (Index _) (ArrValue _)) arr ->
      case (ixRep, valRep) of
        (CRPrim _ ixVal, CRPrim _ valVal) ->
          CRArray d (SBV.writeSArr arr ixVal valVal)

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
  :: Prim p k a
  -> Prim p k b
  -> CoreRepr (PrimS a)
  -> CoreRepr (PrimS b)
coercePrim _ _ _ = error "primitive coercion not implemented yet"
