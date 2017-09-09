{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Annotation where

import           Data.Data

import           Control.Lens

import qualified Language.Fortran.Analysis          as F
import qualified Language.Fortran.AST               as F

import qualified Camfort.Analysis.Annotations       as Ann
import           Camfort.Analysis.CommentAnnotator

import           Camfort.Specification.Hoare.Syntax

type HA = F.Analysis (HoareAnnotation Ann.A)
type InnerHA = F.Analysis Ann.A

data HoareAnnotation a =
  HoareAnnotation
  { _hoarePrevAnnotation  :: a
  , _hoareSod :: Maybe (SpecOrDecl InnerHA)
  , _hoarePUName :: Maybe F.ProgramUnitName
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