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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Camfort.Analysis.Annotations
  (
  -- * Annotation Datatype
    Annotation(..)
  , A
  , unitAnnotation
  -- ** Predicates
  , pRefactored
  -- ** Transformation Helpers
  , onPrev
  -- ** Specification Annotation Helpers
  -- * Other Helpers
  , buildCommentText
  ) where

import Data.Data
import Data.Maybe (isJust)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import Language.Fortran.Version (FortranVersion(Fortran90))
import qualified Language.Fortran.Util.Position as FU

type A = Annotation
data Annotation =
  A { unitVar        :: Int
    , number         :: Int
    , refactored     :: Maybe FU.Position
    -- indicates when a node is newly introduced
    , newNode        :: Bool
    -- indicates a node which is being deleted
    , deleteNode    :: Bool
    } deriving (Eq, Show, Typeable, Data)

-- Predicate on whether an AST has been refactored
pRefactored :: Annotation -> Bool
pRefactored = isJust . refactored

unitAnnotation :: Annotation
unitAnnotation = A
  { unitVar      = 0
   , number       = 0
   , refactored   = Nothing
   , newNode      = False
   , deleteNode   = False
 }

--------------------------------------------------
-- Helpers for transforming the 'previous' annotation
onPrev :: (a -> a) -> FA.Analysis a -> FA.Analysis a
onPrev f ann = ann { FA.prevAnnotation = f (FA.prevAnnotation ann) }

-- | Build a Fortran comment string appropriate for the Fortran version.
buildCommentText :: F.MetaInfo -> Int -> String -> String
buildCommentText mi col text | isModernFortran = replicate col ' ' ++ "!" ++ text
                             | otherwise       = "c" ++ text
  where isModernFortran = F.miVersion mi >= Fortran90
