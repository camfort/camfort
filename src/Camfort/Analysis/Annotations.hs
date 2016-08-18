{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Camfort.Analysis.Annotations where

import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Maybe (isJust)

import Data.Map.Lazy hiding (map)
import Debug.Trace

import Camfort.Specification.Units.Environment
import qualified Camfort.Specification.Units.Parser as P
import Camfort.Analysis.CommentAnnotator
import qualified Camfort.Specification.Stencils.Syntax as StencilSpec
import qualified Camfort.Specification.Stencils.Grammar as StencilComment

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Util.Position as FU

type Report = String

type A = Annotation
data Annotation =
  A { unitVar        :: Int
    , number         :: Int
    , refactored     :: Maybe FU.Position
    -- indicates when a node is newly introduced
    , newNode        :: Bool
    -- indicates a node which is being deleted
    , deleteNode    :: Bool
    -- Stencil specification annotations
    -- TODO: move these into their own annotation
    , stencilSpec    :: Maybe
    -- If defined, either an unprocessed syntax tree
         (Either StencilComment.Specification
           -- Or a parser AST of a RegionEnv or SpecDecls
           (Either StencilSpec.RegionEnv StencilSpec.SpecDecls))
    , stencilBlock   :: Maybe (F.Block (FA.Analysis Annotation))
    } deriving (Eq, Show, Typeable, Data)

-- Predicate on whether an AST has been refactored
pRefactored :: Annotation -> Bool
pRefactored = isJust . refactored

unitAnnotation = A
  { unitVar      = 0
   , number       = 0
   , refactored   = Nothing
   , newNode      = False
   , deleteNode   = False
   , stencilSpec  = Nothing
   , stencilBlock = Nothing
 }

--------------------------------------------------
-- Convenience name for a common annotation type.
type UA = FA.Analysis (UnitAnnotation A)

-- Instances for embedding parsed specifications into the AST
instance ASTEmbeddable UA P.UnitStatement where
  annotateWithAST ann ast =
    onPrev (\ ann -> ann { unitSpec = Just ast }) ann

-- Link annotation comments to declaration statements
instance Linkable UA where
  link ann (b@(F.BlStatement _ _ _ (F.StDeclaration {}))) =
      onPrev (\ ann -> ann { unitBlock = Just b }) ann
  link ann b = ann

-- Helpers for transforming the 'previous' annotation
onPrev :: (a -> a) -> FA.Analysis a -> FA.Analysis a
onPrev f ann = ann { FA.prevAnnotation = f (FA.prevAnnotation ann) }

modifyAnnotation :: F.Annotated f => (a -> a) -> f a -> f a
modifyAnnotation f x = F.setAnnotation (f (F.getAnnotation x)) x
