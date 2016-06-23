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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Camfort.Analysis.StencilSpecification.Synthesis where

import Data.Data
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Camfort.Analysis.StencilSpecification.InferenceBackend
import Camfort.Analysis.StencilSpecification.Syntax
import Camfort.Analysis.StencilSpecification.Model

import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations
import Camfort.Extensions.UnitsForpar (parameterise)
import Camfort.Helpers.Vec
-- These two are redefined here for ForPar ASTs
import Camfort.Helpers hiding (lineCol, spanLineCol)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD

import Language.Fortran.Util.Position

import Data.Map hiding (map)

a = FA.analysis0 unitAnnotation
s = SrcSpan (Position 0 0 0) (Position 0 0 0)

-- Given a spec, an array variable, and a list of inductive variables, generate
-- a list of indexing expressions for the spec
synthesise :: Specification -> F.Name -> [F.Name] -> [F.Expression (FA.Analysis A)]
synthesise (Specification (Left (Exact spec))) v ixs =
  map toSubscriptExpr . toList . fromExact . model $ (Exact spec)
    where toSubscriptExpr (offs,_) = ixExprToSubscript v
                                    . map (uncurry offsetToIx) $ zip ixs offs
synthesise _ _ _ = []

ixExprToSubscript :: F.Name -> [F.Index (FA.Analysis A)] -> F.Expression (FA.Analysis A)
ixExprToSubscript v es =
    F.ExpSubscript a s (F.ExpValue a s (F.ValVariable v)) (F.AList a s es)

-- Make indexing expression for variable 'v' from an offset.
-- essentially inverse to `ixToOffset` in StencilSpecification
offsetToIx :: F.Name -> Int -> F.Index (FA.Analysis A)
offsetToIx v o
  | o == 0    = F.IxSingle a s Nothing (F.ExpValue a s (F.ValVariable v))
  | o  > 0    = F.IxSingle a s Nothing (F.ExpBinary a s F.Addition
                                 (F.ExpValue a s (F.ValVariable v))
                                 (F.ExpValue a s (F.ValInteger $ show o)))
  | otherwise = F.IxSingle a s Nothing (F.ExpBinary a s F.Subtraction
                                 (F.ExpValue a s (F.ValVariable v))
                                 (F.ExpValue a s (F.ValInteger $ show (abs o))))
