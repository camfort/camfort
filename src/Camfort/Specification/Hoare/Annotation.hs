{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wall #-}

{-|

Fortran AST annotations used for Hoare logic checking.

-}
module Camfort.Specification.Hoare.Annotation where

import           Data.Data

import           Control.Lens

import qualified Language.Fortran.Analysis          as F
import qualified Language.Fortran.AST               as F

import qualified Camfort.Analysis.Annotations       as Ann
import           Camfort.Analysis.CommentAnnotator

import           Camfort.Specification.Hoare.Syntax


-- | Annotations meant to appear on the main annotated program's AST.
type HA = F.Analysis (HoareAnnotation Ann.A)


-- | Annotations meant to appear on the AST inside those Fortran expressions
-- that have been parsed from inside logical expression annotations.
type InnerHA = F.Analysis Ann.A

data HoareAnnotation a =
  HoareAnnotation
  { _hoarePrevAnnotation  :: a
  , _hoareSod :: Maybe (SpecOrDecl InnerHA)
  -- ^ A @static_assert@ specification or @decl_aux@ declaration.
  , _hoarePUName :: Maybe F.ProgramUnitName
  -- ^ The name of the program unit that the spec or decl is attached to.
  }
  deriving (Show, Eq, Typeable, Data)

makeLenses ''HoareAnnotation

instance Linkable HA where
  link ann _ = ann

  linkPU ann pu = Ann.onPrev (hoarePUName .~ Just (F.puName pu)) ann

instance ASTEmbeddable HA (SpecOrDecl InnerHA) where
  annotateWithAST ann ast =
    Ann.onPrev (hoareSod .~ Just ast) ann


hoareAnn0 :: a -> HoareAnnotation a
hoareAnn0 x = HoareAnnotation { _hoarePrevAnnotation = x, _hoareSod = Nothing, _hoarePUName = Nothing }
