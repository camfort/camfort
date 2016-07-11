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

module Camfort.Specification.Units.Strip where

import Data.Data
import Data.Char
import Data.Generics.Uniplate.Operations

import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.Synthesis
import Camfort.Transformation.Syntax

{-| DEPRECATED: Provide functionality for stripping out unit annotations -}

{-
removeUnitsInBlock :: Block Annotation -> Block Annotation
removeUnitsInBlock = transformBi deleteUnits

deleteUnits :: Decl Annotation -> Decl Annotation
deleteUnits (Decl a sp@(s1, s2) d t) | hasUnits t =
  Decl a' (dropLine sp) d t'
  where a' = a { refactored = Just $ toCol0 s1 }
        t' = deleteUnit t
deleteUnits (MeasureUnitDef a sp@(s1, s2) d) =
  NullDecl a' sp'
  where a' = a { refactored = Just s1 }
        sp' = (toCol0 s1, snd $ dropLine sp)
deleteUnits decl = decl

deleteUnit :: Type Annotation -> Type Annotation
deleteUnit (BaseType aa tt attrs kind len) =
  BaseType aa tt (filter (not . isUnit) attrs) kind len
deleteUnit (ArrayT dims aa tt attrs kind len) =
  ArrayT dims aa tt (filter (not . isUnit) attrs) kind len
-}