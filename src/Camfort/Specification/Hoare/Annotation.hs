{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Annotation where

import Data.Data

import Control.Lens

import qualified Language.Fortran.Analysis          as F

import           Camfort.Analysis.Annotations (onPrev)
import           Camfort.Analysis.CommentAnnotator

import           Camfort.Specification.Hoare.Syntax

data Annotation a =
  A
  { _annHoareSpec :: Maybe (Specification a)
  }
  deriving (Show, Eq, Typeable, Data, Functor)

makeLenses ''Annotation

type A = Annotation

type Analysis a = F.Analysis (A a)

instance Linkable (Analysis a) where
  link ann _ = ann
  linkPU ann _ = ann

instance ASTEmbeddable (Analysis a) (Specification a) where
  annotateWithAST ann ast =
    onPrev (annHoareSpec .~ Just ast) ann
