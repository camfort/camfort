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

{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleContexts, ImplicitParams,
    TupleSections, FunctionalDependencies #-}

module Camfort.Analysis.StencilSpecification.Check where

import Data.Data
import Data.List
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product,Sum)

import Camfort.Analysis.StencilSpecification.Inference
import Camfort.Analysis.StencilSpecification.Syntax
import Camfort.Analysis.StencilSpecification.Model
import qualified Camfort.Analysis.StencilSpecification.Grammar as SYN

import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations
import Camfort.Extensions.UnitsForpar (parameterise)
import Camfort.Helpers.Vec
import Camfort.Helpers hiding (lineCol, spanLineCol) -- These two are redefined here for ForPar ASTs

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD

import Language.Fortran.Util.Position

import Data.Set hiding (map)

-- Class for functions converting from Grammar parse
-- syntax to the AST representation of the Syntax module
class SynToAst s t | s -> t where
  synToAst :: s -> t

instance SynToAst SYN.Spec Specification where
  synToAst (SYN.Spatial mods r) = Specification $ Left $
    case approx of
      Just SYN.AtMost  -> Bound Nothing (Just s')
      Just SYN.AtLeast -> Bound (Just s') Nothing
      Nothing          -> Exact s'
    where s' = Spatial modLinear modIrrefl modRefl (dnf r)
          (modLinear, modIrrefl, modRefl, approx) = synToAst mods

-- Convert a grammar syntax to Disjunctive Normal Form AST
dnf :: SYN.Region -> RegionSum

-- Distributive law
dnf (SYN.And r1 r2) =
    Sum $ (unSum $ dnf r1) >>= (\(Product ps1) ->
            (unSum $ dnf r2) >>= (\(Product ps2) ->
               return $ Product $ ps1 ++ ps2))
-- Coalesce sums
dnf (SYN.Or r1 r2) =
    Sum $ (unSum $ dnf r1) ++ (unSum $ dnf r2)
-- Region conversion
dnf (SYN.Forward dep dim)  = Sum [Product [Forward dep dim]]
dnf (SYN.Backward dep dim) = Sum [Product [Backward dep dim]]
dnf (SYN.Centered dep dim) = Sum [Product [Centered dep dim]]
dnf (SYN.Var v)            = error "TODO ?"

-- Convert modifier list to modifier info
instance SynToAst [SYN.Mod]
                  (Linearity, [Dimension], [Dimension], Maybe SYN.Mod) where
  synToAst mods = (linearity, irrefls, refls, approx)
    where
      linearity = if elem SYN.ReadOnce mods then Linear else NonLinear

      irrefls = maybe [] id (find' isIrrefl mods)
      isIrrefl (SYN.Irreflexive ds) = Just $ ds
      isIrrefl _                    = Nothing

      approx = find' isApprox mods
      isApprox SYN.AtMost  = Just $ SYN.AtMost
      isApprox SYN.AtLeast = Just $ SYN.AtLeast
      isApprox _           = Nothing

      refls = maybe [] id (find' isRefl mods)
      isRefl (SYN.Reflexive ds) = Just $ ds
      isRefl _                  = Nothing

find' :: Eq a => (a -> Maybe b) -> [a] -> Maybe b
find' p [] = Nothing
find' p (x : xs) =
  case p x of
    Nothing -> find' p xs
    Just b  -> Just b