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

{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleContexts, ImplicitParams, TupleSections #-}

module Camfort.Analysis.StencilSpecification.Synthesis where

import Data.Data
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Camfort.Analysis.StencilSpecification.Inference
import Camfort.Analysis.StencilSpecification.Syntax
import Camfort.Analysis.StencilSpecification.Model

import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations
import Camfort.Extensions.UnitsForpar (parameterise)
import Camfort.Helpers.Vec
import Camfort.Helpers hiding (lineCol, spanLineCol) -- These two are redefined here for ForPar ASTs

import qualified Forpar.AST as F
import qualified Forpar.Analysis as FA
import qualified Forpar.Analysis.Types as FAT
import qualified Forpar.Analysis.Renaming as FAR
import qualified Forpar.Analysis.BBlocks as FAB
import qualified Forpar.Analysis.DataFlow as FAD

import Forpar.Util.Position

import Data.Map hiding (map)

a = unitAnnotation
s = SrcSpan (Position 0 0 0) (Position 0 0 0)

-- Given a spec, an array variable, and a list of inductive variables, generate
-- a list of indexing expressions for the spec
synthesise :: Specification -> F.Name -> [F.Name] -> [F.Expression Annotation]
synthesise spec v ixs = map toArrSubsExpr . toList . model $ spec
  where toArrSubsExpr (offs, linearity) = ixExprToSubscript v . map (uncurry offsetToIxExpr) $ zip ixs offs

ixExprToSubscript :: F.Name -> [F.Expression Annotation] -> F.Expression Annotation
ixExprToSubscript v es = 
  F.ExpSubscript a s (F.ExpValue a s (F.ValArray a v)) (F.AList a s es)

-- Make indexing expression for variable 'v' from an offset.
-- essentially inverse to `ixExprToOffset` in StencilSpecification
offsetToIxExpr :: F.Name -> Int -> F.Expression Annotation
offsetToIxExpr v o 
  | o == 0    = F.ExpValue a s (F.ValVariable a v)
  | o  > 0    = F.ExpBinary a s F.Addition
                  (F.ExpValue a s (F.ValVariable a v))
                  (F.ExpValue a s (F.ValInteger $ show o))
  | otherwise = F.ExpBinary a s F.Subtraction
                  (F.ExpValue a s (F.ValVariable a v))
                  (F.ExpValue a s (F.ValInteger $ show (abs o)))
                  