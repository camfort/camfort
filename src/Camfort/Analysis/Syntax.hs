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
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, KindSignatures,
             FlexibleContexts, GADTs, DeriveGeneric #-}

{-|

This module provides a number of helper functions for working with Fortran syntax that are useful
between different analyses and transformations.

-}
module Camfort.Analysis.Syntax where

-- Standard imports
import Data.Char
import Data.List
import Data.Monoid
import Control.Monad.State.Lazy
import Debug.Trace

-- Data-type generics imports
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Zipper
import Data.Typeable

-- CamFort specific functionality
import Camfort.Analysis.Annotations
import Camfort.Traverse

import qualified Language.Fortran.AST as F
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter

-- * Comparison and ordering

{-|  'AnnotationFree' is a data type that wraps other types and denotes terms
     which should  be compared for equality modulo their annotations and source
     location information -}
data AnnotationFree t = AnnotationFree { annotationBound :: t } deriving Show

{-| short-hand constructor for 'AnnotationFree' -}
af = AnnotationFree
{-| short-hand deconstructor for 'AnnotationFree' -}
unaf = annotationBound

lower = map toLower

-- Here begins varioous 'Eq' instances for instantiations of 'AnnotationFree'

instance Eq (AnnotationFree a) => Eq (AnnotationFree [a]) where
    (AnnotationFree xs) == (AnnotationFree xs') =
     if (length xs == length xs')
     then foldl (\b -> \(x, x') -> ((af x) == (af x')) && b) True (zip xs xs')
     else False

instance Eq (AnnotationFree Int) where
    x == y = (unaf x) == (unaf y)

instance Eq (AnnotationFree Char) where
    x == y = (unaf x) == (unaf y)

instance (Eq (AnnotationFree a), Eq (AnnotationFree b))
      => Eq (AnnotationFree (a, b)) where

    (AnnotationFree (x, y)) == (AnnotationFree (x', y')) =
        ((af x) == (af x')) && ((af y) == (af y'))

instance Eq a => Eq (AnnotationFree (F.Expression a)) where
    (AnnotationFree x) == (AnnotationFree y) = x == y''
        where y'' = setSecondParameter (getSecondParameter x) y'
              y' = setFirstParameter (getFirstParameter x) y

-- * Accessor functions for extracting various pieces of information
--    out of syntax trees
{-| Extracts a string of the (root) variable name from a variable expression
   (if it is indeed a variable expression -}
varExprToVariable :: F.Expression a -> Maybe F.Name
varExprToVariable (F.ExpValue _ _ (F.ValVariable v)) = Just v
varExprToVariable _                                  = Nothing

{-| Set a default monoid instances for Int -}
instance Monoid Int where
    mempty = 0
    mappend = (+)

