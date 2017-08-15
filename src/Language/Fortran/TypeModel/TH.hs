{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell         #-}

{-# OPTIONS_GHC -Wall #-}

{-|

Models the base of the Fortran type system in Haskell types.

-}
module Language.Fortran.TypeModel.TH where

import           Data.SBV
import           Data.Coerce (coerce)

import Language.Haskell.TH

import Language.Fortran.TypeModel.SBV


symInstances :: Name -> Q [Dec]
symInstances tyName = do
  info <- reify tyName

  tyDec <- case info of
    TyConI d -> return d
    _ -> fail $ show tyName ++ " does not reference a type"

  con <- case tyDec of
    NewtypeD _ _ [] _ c _ -> return c
    _ -> fail $ show tyName ++ " does not reference a newtype"

  (conName, conTy) <- case con of
    NormalC n [(_, ty)] -> return (n, ty)
    RecC n [(_, _, ty)] -> return (n, ty)
    _ -> fail $ show tyName ++ " is not a simple newtype"

  symInstancesFor tyName conName conTy

symInstancesFor :: Name -> Name -> Type -> DecsQ
symInstancesFor tyName conName conTy =
  [d|
   instance Read $(conT tyName) where
     readsPrec p x = coerce (readsPrec p x :: [($(return conTy), String)])

   instance Show $(conT tyName) where
     showsPrec p x = showsPrec p (coerce x :: $(return conTy))

   instance HasKind $(conT tyName) where
     kindOf $(conP conName [[p|x|]]) = kindOf x

   instance SymWord $(conT tyName) where
     literal $(conP conName [[p|x|]]) = wrapSym (literal x)
     fromCW = $(conE conName) . fromCW
     mkSymWord q n = wrapSym <$> (mkSymWord q n :: Symbolic (SBV $(return conTy)))

   instance WrappedSym $(conT tyName) where
     type UnwrappedSym $(conT tyName) = $(return conTy)
     wrapSym = unsafeTransmuteSBV
     unwrapSym = unsafeTransmuteSBV
   |]

symWrapper :: Name -> String -> [Name] -> Q [Dec]
symWrapper wrappedTyName wrapperNameS extraDerivs = do
  info <- reify wrappedTyName

  wrappedTy <- case info of
    TyConI _ -> return (ConT wrappedTyName)
    _ -> fail $ show wrappedTyName ++ " does not reference a type"

  let wrapperName = mkName wrapperNameS

  let allDerivs = [''Eq, ''Ord] ++ extraDerivs

  let derivClause = traverse conT allDerivs

  ty <-
    newtypeD
    (return [])
    wrapperName []
    Nothing
    (return (NormalC wrapperName [(Bang NoSourceUnpackedness NoSourceStrictness, wrappedTy)]))
    derivClause

  instances <- symInstancesFor wrapperName wrapperName wrappedTy

  return (ty : instances)
