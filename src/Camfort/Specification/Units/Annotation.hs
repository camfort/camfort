{- |
Module      :  Camfort.Specification.Units.Annotation
Description :  Annotation with unit information.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental

Defines the 'UnitAnnotation' datatype, which is used for annotating a
'ProgramFile' with units information.
-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Camfort.Specification.Units.Annotation
  (
    -- * Annotation Type
    UA
  , mkUnitAnnotation
  , prevAnnotation
  , unitBlock
  , unitConstraint
  , unitInfo
  , unitPU
  , unitSpec
    -- * Helpers
  , cleanLinks
  , getConstraint
  , getUnitInfo
  , maybeSetUnitConstraintF2
  , maybeSetUnitInfo
  , maybeSetUnitInfoF2
  , setConstraint
  , setUnitInfo
  ) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Operations (transformBi)

import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.AST      as F

import qualified Camfort.Analysis.Annotations             as Ann
import           Camfort.Analysis.CommentAnnotator
  (ASTEmbeddable(..), Linkable(..))
import qualified Camfort.Specification.Units.Environment  as E
import qualified Camfort.Specification.Units.Parser.Types as P

-- The annotation on the AST used for solving units.
data UnitAnnotation a = UnitAnnotation {
    prevAnnotation :: a,
    unitSpec       :: Maybe P.UnitStatement,
    unitConstraint :: Maybe E.Constraint,
    unitInfo       :: Maybe E.UnitInfo,
    unitBlock      :: Maybe (F.Block (FA.Analysis (UnitAnnotation a))), -- ^ linked variable declaration
    unitPU         :: Maybe (F.ProgramUnit (FA.Analysis (UnitAnnotation a))) -- ^ linked program unit
  } deriving (Data, Typeable, Show)

mkUnitAnnotation :: a -> UnitAnnotation a
mkUnitAnnotation a = UnitAnnotation a Nothing Nothing Nothing Nothing Nothing

-- Convenience name for a common annotation type.
type UA = FA.Analysis (UnitAnnotation Ann.A)

-- Instances for embedding parsed specifications into the AST
instance ASTEmbeddable UA P.UnitStatement where
  annotateWithAST ann ast =
    Ann.onPrev (\ann' -> ann' { unitSpec = Just ast }) ann

-- Link annotation comments to declaration statements
instance Linkable UA where
  link ann (b@(F.BlStatement _ _ _ F.StDeclaration {})) =
      Ann.onPrev (\ann' -> ann' { unitBlock = Just b }) ann
  link ann _ = ann
  linkPU ann pu@F.PUFunction{} =
      Ann.onPrev (\ann' -> ann' { unitPU = Just pu }) ann
  linkPU ann pu@F.PUSubroutine{} =
      Ann.onPrev (\ann' -> ann' { unitPU = Just pu }) ann
  linkPU ann _ = ann

-- | Extract the unit info from a given annotated piece of AST.
getUnitInfo :: F.Annotated f => f UA -> Maybe E.UnitInfo
getUnitInfo = unitInfo . FA.prevAnnotation . F.getAnnotation

-- | Extract the constraint from a given annotated piece of AST.
getConstraint :: F.Annotated f => f UA -> Maybe E.Constraint
getConstraint = unitConstraint . FA.prevAnnotation . F.getAnnotation

-- | Set the UnitInfo field on a piece of AST.
setUnitInfo :: F.Annotated f => E.UnitInfo -> f UA -> f UA
setUnitInfo info = F.modifyAnnotation (Ann.onPrev (\ ua -> ua { unitInfo = Just info }))

-- | Set the Constraint field on a piece of AST.
setConstraint :: F.Annotated f => E.Constraint -> f UA -> f UA
setConstraint (E.ConConj []) = id
setConstraint c              =
  F.modifyAnnotation (Ann.onPrev (\ ua -> ua { unitConstraint = Just c }))

--------------------------------------------------

-- Various helper functions for setting the UnitInfo or Constraint of a piece of AST
maybeSetUnitInfo :: F.Annotated f => Maybe E.UnitInfo -> f UA -> f UA
maybeSetUnitInfo u e  = maybe e (`setUnitInfo` e) u

maybeSetUnitInfoF2 :: F.Annotated f => (a -> b -> E.UnitInfo) -> Maybe a -> Maybe b -> f UA -> f UA
maybeSetUnitInfoF2 f u1 u2 e = maybe e (`setUnitInfo` e) (f <$> u1 <*> u2)

maybeSetUnitConstraintF2 :: F.Annotated f => (a -> b -> E.Constraint) -> Maybe a -> Maybe b -> f UA -> f UA
maybeSetUnitConstraintF2 f u1 u2 e = maybe e (`setConstraint` e) (f <$> u1 <*> u2)

cleanLinks :: F.ProgramFile UA -> F.ProgramFile UA
cleanLinks = transformBi
  (\a -> a { unitPU    = Nothing
           , unitBlock = Nothing
           , unitSpec  = Nothing
           } :: UnitAnnotation Ann.A)
