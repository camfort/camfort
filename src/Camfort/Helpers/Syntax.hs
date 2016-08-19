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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{-|

This module provides a number of helper functions for working with Fortran
syntax that are useful between different analyses and transformations.

-}
module Camfort.Helpers.Syntax where

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

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU
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

-- variable renaming helpers
caml (x:xs) = toUpper x : xs
lower = map toLower

-- Here begins varioous 'Eq' instances for instantiations of 'AnnotationFree'

instance Eq (AnnotationFree a) => Eq (AnnotationFree [a]) where
    (AnnotationFree xs) == (AnnotationFree xs') =
     if length xs == length xs'
     then foldl (\b (x, x') -> (af x == af x') && b) True (zip xs xs')
     else False

instance (Eq (AnnotationFree a), Eq (AnnotationFree b))
      => Eq (AnnotationFree (a, b)) where

    (AnnotationFree (x, y)) == (AnnotationFree (x', y')) =
        (af x == af x') && (af y == af y')

instance Eq a => Eq (AnnotationFree (F.Expression a)) where
    (AnnotationFree x) == (AnnotationFree y) = x'' == y''
        where x' = fmap (const ()) x
              y' = fmap (const ()) y
              y'' = transformBi setSpanConst y'
              x'' = transformBi setSpanConst x'
              setSpanConst :: FU.SrcSpan -> FU.SrcSpan
              setSpanConst (FU.SrcSpan _ _) = FU.SrcSpan pos0 pos0
                 where pos0 = FU.Position 0 0 0

instance Eq (AnnotationFree F.BaseType) where
    (AnnotationFree x) == (AnnotationFree y) = x == y

-- * Accessor functions for extracting various pieces of information
--    out of syntax trees
{-| Extracts a string of the (root) variable name from an expression,
    e.g., extractVariable "v"    = Just v
          extractVariable "v(i)" = Just v -}
extractVariable :: F.Expression a -> Maybe F.Name
extractVariable (F.ExpValue _ _ (F.ValVariable v)) = Just v
extractVariable (F.ExpSubscript _ _ e _)           = extractVariable e
extractVariable _                                  = Nothing

{-| Set a default monoid instances for Int -}
instance Monoid Int where
    mempty = 0
    mappend = (+)

-- SrcSpan helpers

dropLine :: FU.SrcSpan -> FU.SrcSpan
dropLine (FU.SrcSpan s1 (FU.Position o c l)) =
    FU.SrcSpan s1 (FU.Position o 0 (l+1))

deleteLine :: FU.SrcSpan -> FU.SrcSpan
deleteLine (FU.SrcSpan (FU.Position ol cl ll) (FU.Position ou cu lu)) =
    FU.SrcSpan (FU.Position ol (cl-1) ll) (FU.Position ou 0 (lu+1))

linesCovered :: FU.Position -> FU.Position -> Int
linesCovered (FU.Position _ _ l1) (FU.Position _ _ l2) = l2 - l1 + 1

toCol0 (FU.Position o c l) = FU.Position o 0 l

afterAligned :: FU.SrcSpan -> FU.Position
afterAligned (FU.SrcSpan (FU.Position o cA lA) (FU.Position _ cB lB)) =
    FU.Position o cA (lB+1)